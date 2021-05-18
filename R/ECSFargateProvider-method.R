#' Create an ECS Fargate cloud provider
#'
#' Create an ECS Fargate cloud provider. Note that the arguments for the function
#' are the settings for ECS fargate. You can find them in the AWS ECS console.
#'
#' @param clusterName Character, the cluster name in ECS Fargate
#' @param serverTaskDefName,workerTaskDefName Character, the task defintion name for
#' the server and worker
#' @param securityGroupName Character, the security group name
#' @param vpcId,subnetId,securityGroupId,internetGatewayId,routeTableId,taskExecRoleId The ID/ARN
#' of the resources for the container
#' @param enableWorkerPublicIp Logical, whether to enable the public IP for the worker.
#' If this value is `FALSE`, the worker will not be able to pull the container image from the
#' public network
#' @param logDriver Character, the log driver, if `logDriver = "auto"`, the awslogs will be used
#' @param logOptions Named list, the options for the log driver
#' @param region Character, the region of the ECS computing cluster
#' @examples
#' provider <- ECSFargateProvider()
#' @return a `ECSFargateProvider` object
#' @export
ECSFargateProvider <- function(clusterName = "R-worker-cluster",
                               serverTaskDefName = "R-server-task-definition",
                               workerTaskDefName = "R-worker-task-definition",
                               securityGroupName = "R-parallel-security-group",
                               vpcId = NULL,
                               subnetId = NULL,
                               securityGroupId = NULL,
                               internetGatewayId = NULL,
                               routeTableId = NULL,
                               taskExecRoleId = NULL,
                               enableWorkerPublicIp = TRUE,
                               logDriver = c("auto", "none", "awslogs", "awsfirelens","splunk"),
                               logOptions = list(),
                               region = aws.ecx::aws_get_region()){

    .ECSFargateProvider$new(
        clusterName = clusterName,
        serverTaskDefName = serverTaskDefName,
        workerTaskDefName = workerTaskDefName,
        securityGroupName = securityGroupName,
        vpcId = vpcId,
        subnetId = subnetId,
        securityGroupId = securityGroupId,
        internetGatewayId = internetGatewayId,
        routeTableId = routeTableId,
        enableWorkerPublicIp=enableWorkerPublicIp,
        taskExecRoleId = taskExecRoleId,
        clusterNameVerified = FALSE,
        serverTaskDefNameVerified = FALSE,
        workerTaskDefNameVerified = FALSE,
        securityGroupVerified = FALSE,
        vpcVerified = FALSE,
        subnetVerified = FALSE,
        internetGatewayVerified = FALSE,
        routeTableVerified = FALSE,
        routeVerified = FALSE,
        inboundPermissionVerified = FALSE,
        taskExecRoleVerified = FALSE,
        initialized = FALSE,
        logDriver = match.arg(logDriver),
        logOptions = logOptions,
        region = region
    )
}

#' Show the `ECSFargateProvider` object
#'
#' Show the `ECSFargateProvider` object
#'
#' @param object The `ECSFargateProvider` object
#' @examples
#' provider <- ECSFargateProvider()
#' show(provider)
#' @return No return value
#' @export
setMethod("show", "ECSFargateProvider", function(object){
    cat("Region:              ", object$region, "\n")
    cat("Cluster name:        ", object$clusterName, "\n")
    cat("Server task definition:     ", object$serverTaskDefName, "\n")
    cat("Worker task definition:     ", object$workerTaskDefName, "\n")
    cat("Security group name: ", object$securityGroupName, "\n")

    if(!is.null(object$vpcId)){
        cat("VPC ID:              ", object$vpcId, "\n")
    }
    if(!is.null(object$subnetId)){
        cat("Subnet ID:           ", object$subnetId, "\n")
    }
    if(!is.null(object$securityGroupId)){
        cat("Security group ID:   ", object$securityGroupId, "\n")
    }
    if(!is.null(object$internetGatewayId)){
        cat("Internet gateway ID: ", object$internetGatewayId, "\n")
    }
    if(!is.null(object$routeTableId)){
        cat("Route table ID:      ", object$routeTableId, "\n")
    }
    invisible(NULL)
})


