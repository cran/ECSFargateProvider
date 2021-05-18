#' The ECS fargate provider
#'
#' The ECS fargate provider, see `?ECSFargateProvider`
#'
#' @slot clusterName Character, the name of the ECS cluster
#' @slot initialized Logical, whether the provider has been initialized
.ECSFargateProvider <- setRefClass(
    "ECSFargateProvider",
    fields = list(
        clusterName = "CharOrNULL",
        serverTaskDefName = "CharOrNULL",
        workerTaskDefName = "CharOrNULL",
        securityGroupName = "CharOrNULL",
        vpcId = "CharOrNULL",
        subnetId = "CharOrNULL",
        securityGroupId = "CharOrNULL",
        internetGatewayId = "CharOrNULL",
        routeTableId = "CharOrNULL",
        taskExecRoleId = "CharOrNULL",
        enableWorkerPublicIp = "logical",
        clusterNameVerified = "logical",
        serverTaskDefNameVerified = "logical",
        workerTaskDefNameVerified = "logical",
        securityGroupVerified = "logical",
        vpcVerified = "logical",
        subnetVerified = "logical",
        internetGatewayVerified = "logical",
        routeTableVerified = "logical",
        routeVerified = "logical",
        inboundPermissionVerified = "logical",
        taskExecRoleVerified = "logical",
        initialized = "logical",
        logDriver = "character",
        logOptions = "list",
        region = "character"
    ),
    contains = "CloudProvider"
)
