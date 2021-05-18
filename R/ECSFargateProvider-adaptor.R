#' Initialize the Fargate provider
#'
#' Initialize the Fargate provider
#'
#' @inheritParams DockerParallel::initializeProvider
#'
#' @return No return value
#' @export
setMethod("initializeProvider", "ECSFargateProvider", function(provider, cluster, verbose){
    if(!provider$initialized){
        if(!existCredentials()){
            stop(
                paste0("The AWS credentials do not exist, please set them via <aws.ecx::aws_set_credentials>\n",
                       "The general information can be found at https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html")
            )
        }
        if(!cluster$isServerRunning()){
            .setServerWorkerSameLAN(cluster, TRUE)
        }
        verbosePrint(verbose, "Initializing the ECS provider")
        ## Cluster name
        verbosePrint(verbose>1, "\tSetting up cluster")
        clusterName <- configClusterName(provider, region = provider$region)
        verbosePrint(verbose>1, "\tCluster name: \t", clusterName)
        ## VPC
        verbosePrint(verbose>1, "\tSetting up VPC")
        VPCId <- configVpcId(provider, region = provider$region)
        verbosePrint(verbose>1, "\tVPC: \t", VPCId)
        ## subnet
        verbosePrint(verbose>1, "\tSetting up subnet")
        subnetId <- configSubnetId(provider, region = provider$region)
        verbosePrint(verbose>1, "\tSubnet id: \t", subnetId)
        ## gateway
        verbosePrint(verbose>1, "\tSetting up gateway")
        gatewayId <- configInternetGateway(provider, region = provider$region)
        verbosePrint(verbose>1, "\tGateway: \t", gatewayId)
        ## route table
        verbosePrint(verbose>1, "\tSetting up route table")
        routeTableId <- configRouteTable(provider, region = provider$region)
        verbosePrint(verbose>1, "\tRoute table: \t", routeTableId)
        ## route
        verbosePrint(verbose>1, "\tSetting up default route")
        configDefaultRoute(provider, region = provider$region)
        verbosePrint(verbose>1, "\tDefault route finished")
        ## security group
        verbosePrint(verbose>1, "\tSetting up security group")
        securityGroupId <- configSecurityGroup(provider, region = provider$region)
        verbosePrint(verbose>1, "\tSecurity group: ",securityGroupId)
        # Inbound permission
        verbosePrint(verbose>1, "\tSetting up SSH and server-worker inbound permission")
        port <- c(22, cluster@cloudConfig$serverPort)
        ConfigInboundPermissions(x = provider, ports = port, region = provider$region)
        verbosePrint(verbose>1, "\tInbound permission finished")

        # Task execution role
        verbosePrint(verbose>1, "\tSetting up task execution role")
        roleArn <- configTaskExecRole(provider)
        verbosePrint(verbose>1, "\tTask execution role:", roleArn)
        # Task definition
        verbosePrint(verbose>1, "\tSetting up task defintion")
        configTaskDefinition(provider, cluster, region = provider$region)
        verbosePrint(verbose>1, "\tTask defintion finished")
        provider$initialized <- TRUE
    }
    invisible(NULL)
})



#' Run the server container
#'
#' Run the server and return the server instance handle.
#' The function will set the environment variable `ECSFargateCloudConfigInfo` to the container
#'
#' @inheritParams DockerParallel::runDockerServer
#'
#' @return The server handle in character
#' @export
setMethod("runDockerServer", "ECSFargateProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
              verbosePrint(verbose>0, "Deploying server container")

              ## save the cloud config to the running instance
              encodedCloudConfig <- encodeCloudConfig(.getCloudConfig(cluster))
              container <- container$copy()
              container$environment[["ECSFargateCloudConfigInfo"]] <- encodedCloudConfig

              fargateHardware <- getValidFargateHardware(hardware)
              if(verbose){
                  informUpgradedHardware(fargateHardware, hardware, 1)
              }
              taskDefName <- provider$serverTaskDefName
              instanceId <- ecsTaskScheduler(
                  provider = provider,
                  taskDefName=taskDefName,
                  container=container,
                  hardware= fargateHardware,
                  containerNum=1,
                  publicIpEnable=TRUE
              )

              if(is.null(instanceId)){
                  stop("Fail to deploy the ECS container, something is wrong")
              }
              instanceId
          })


#' Run the worker container
#'
#' Run the workers and return a list of worker instance handles.
#' The function will set the environment variable `ECSFargateCloudJobQueueName`,
#' `ECSFargateCloudServerIP` and `ECSFargateCloudWorkerNumber` to the container
#'
#' @inheritParams DockerParallel::runDockerWorkers
#'
#' @return A list of worker handle in character
#' @export
setMethod("runDockerWorkers", "ECSFargateProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose = 0L){
              verbosePrint(verbose>0, "Deploying worker container")
              instanceIds <- c()
              maxWorkers <- getMaxWorkerPerContainer(hardware)
              maxWorkers <- min(container$maxWorkerNum, maxWorkers)
              ## run the containers which have the maximum worker number
              containerWithMaxWorker <- floor(workerNumber/maxWorkers)
              if(containerWithMaxWorker>0){
                  instances <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=containerWithMaxWorker,
                      workerPerContainer=maxWorkers,
                      verbose=verbose
                  )
                  if(length(instances)!= containerWithMaxWorker){
                      stopTasks(provider$clusterName, instances)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
                  instanceIds<-c(instanceIds, instances)
              }
              ## Run the container which does not have the maximum worker number
              lastContainerWorkerNum <- workerNumber - maxWorkers*containerWithMaxWorker
              if(lastContainerWorkerNum!=0){
                  instance <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=1,
                      workerPerContainer=lastContainerWorkerNum,
                      verbose=verbose)
                  instanceIds <-c (instanceIds, instance)
                  if(length(instance)!=1){
                      stopTasks(provider$clusterName, instance)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
              }

              ## Repeat the instance id worker number times.
              workerNumberPerContainer <- rep(maxWorkers, length(instanceIds))
              if(lastContainerWorkerNum!=0){
                  workerNumberPerContainer[length(instanceIds)] <- lastContainerWorkerNum
              }
              repeatVector(instanceIds, workerNumberPerContainer)
          }
)

#' Get the instance public/private IPs
#'
#' Get the instance public/private IPs
#'
#' @inheritParams DockerParallel::getDockerInstanceIps
#'
#' @returns
#' A data.frame with publicIp and privateIp columns and each row corresponds to
#' an element in instanceHandles.
#' @export
setMethod("getDockerInstanceIps", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              while(TRUE){
                  taskInfo <- getTaskDetails(provider$clusterName,
                                             taskIds = instanceHandles,
                                             getIP = TRUE)
                  if(taskInfo$publicIp!=""&&taskInfo$privateIp!=""){
                      break
                  }
                  if(taskInfo$status=="STOPPED"){
                      stop("The server has been stopped")
                  }
              }
              taskInfo
          }
)

#' Get the instance status
#'
#' Get the instance status
#'
#' @inheritParams DockerParallel::getDockerInstanceStatus
#'
#' @returns
#' A character vector with each element corresponding to an instance in instanceHandles.
#' Each element must be one of three possible characters "initializing", "running" or "stopped"
#' @export
setMethod("getDockerInstanceStatus", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              uniqueHandles <- unique(instanceHandles)
              taskInfo <- getTaskDetails(provider$clusterName, taskIds = uniqueHandles)
              instanceStatus <- rep("initializing", length(uniqueHandles))
              instanceStatus[taskInfo$status == "RUNNING"] <- "running"
              instanceStatus[taskInfo$status == "STOPPED"] <- "stopped"
              result <- rep("", length(instanceHandles))
              for(i in seq_along(uniqueHandles)){
                  idx <- vapply(instanceHandles, function(x) identical(x, uniqueHandles[[i]]),logical(1))
                  result[idx] <- instanceStatus[i]
              }
              result
          }
)

#' Kill the instances
#'
#' Kill the instances
#'
#' @inheritParams DockerParallel::killDockerInstances
#'
#' @return A logical vector indicating whether the killing operation is success for each instance
#' @export
setMethod("killDockerInstances", "ECSFargateProvider",
          function(provider, instanceHandles, verbose = 0L){
              stopTasks(provider$clusterName, taskIds = unique(instanceHandles))
          }
)


#' Whether the cluster is running on the cloud?
#'
#' Whether the cluster is running on the cloud?
#' This function will check the instances in the ECS cluster defined by
#' `provider$clusterName` and find if there is any instance that has the same job queue
#' name as `.getJobQueueName(cluster)`
#'
#' @inheritParams DockerParallel::dockerClusterExists
#'
#' @return A logical value
#' @export
setMethod("dockerClusterExists", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              ## Check if the cluster exists
              clusterList <- listClusters()
              if(!provider$clusterName%in%clusterList){
                  return(FALSE)
              }

              ## Check if the server exists
              serverHandles <- listRunningServer(cluster)
              serverInfo <- findServerInfo(cluster, serverHandles)
              !is.null(serverInfo)
          }
)


#' Reconnect the cluster
#'
#' Reconnect the cluster. This function will check the instances in
#' the ECS cluster defined by `provider$clusterName` and find if there is any
#' instance that has the same job queue name as `.getJobQueueName(cluster)`. If
#' so, the function can reconnect the cluster.
#'
#' @inheritParams DockerParallel::reconnectDockerCluster
#'
#' @return No return value
#' @export
setMethod("reconnectDockerCluster", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              cloudConfig <- .getCloudConfig(cluster)
              serverHandles <- listRunningServer(cluster)
              serverInfo <- findServerInfo(cluster, serverHandles)
              if(!is.null(serverInfo)){
                  ## Set the cloud config
                  serverHandle <- serverInfo$handle
                  cloudConfigValue <- serverInfo$cloudConfigValue
                  lapply(names(cloudConfigValue), function(i)
                      cloudConfig$field(i, cloudConfigValue[[i]]))

                  ## Set the server runtime
                  serverDetails <-
                      getTaskDetails(provider$clusterName,taskIds = serverHandle, getIP = TRUE)
                  .setServerHandle(cluster, serverHandle)
                  .setServerPublicIp(cluster, serverDetails$publicIp)
                  .setServerPrivateIp(cluster, serverDetails$privateIp)

                  ## Set the worker runtime
                  workerHandles <- findWorkerHandles(cluster, cloudConfigValue$jobQueueName)
                  .addWorkerHandles(cluster, workerHandles)
                  .setWorkerNumber(cluster, length(workerHandles))
              }
          }
)



