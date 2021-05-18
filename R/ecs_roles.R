defaultRoleName <- "DockerParallelCloudWatchRules"
defaultPolicyName <- "CloudWatchWriteAndContainerRegistryRead"
policyTemplate <- list(
    Version = "2012-10-17",
    Statement = list(list(Sid = "VisualEditor0",
                          Effect = "Allow",
                          Action = c("ecr:GetRegistryPolicy", "ecr:DescribeImageScanFindings",
                                     "ecr:GetLifecyclePolicyPreview", "ecr:DescribeRegistry",
                                     "ecr:GetDownloadUrlForLayer", "ecr:GetAuthorizationToken",
                                     "logs:CreateLogGroup", "logs:PutLogEvents", "logs:CreateLogStream",
                                     "ecr:BatchGetImage", "ecr:DescribeImages", "ecr:BatchCheckLayerAvailability",
                                     "ecr:GetRepositoryPolicy", "ecr:GetLifecyclePolicy"),
                          Resource = "*")))

createRole <- function(roleName = defaultRoleName){
    assumeRolePolicyDocument <- "{\"Version\":\"2008-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ecs-tasks.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}"
    iamRole <- aws.iam::create_role(
        role = roleName,
        policy = assumeRolePolicyDocument,
        region = "us-east-1",
        key = aws.ecx::aws_get_access_key_id(),
        secret = aws.ecx::aws_get_secret_access_key())
    roleId <- iamRole$CreateRoleResponse$CreateRoleResult$Role$Arn
    roleId
}


attachDefaultPolicy <- function(roleName){
    policy <- as.character(jsonlite::toJSON(policyTemplate, auto_unbox =TRUE))
    aws.iam::add_policy(role = roleName,
                        policy = defaultPolicyName,
                        doc = policy,
                        region = "us-east-1")
}
listRoles <- function(){
    roles <- aws.iam::list_roles(
        region = "us-east-1",
        key = aws.ecx::aws_get_access_key_id(),
        secret = aws.ecx::aws_get_secret_access_key())
    roleName <- vapply(roles, function(x)x$RoleName, character(1))
    roleArn <- vapply(roles, function(x)x$Arn, character(1))
    data.frame(roleName = roleName, roleArn = roleArn)
}

listPolicies <- function(roleName){
    response <-aws.iam::list_policies(role = roleName, region = "us-east-1")
    vapply(response, function(x)x$PolicyName, character(1))
}

configTaskExecRole <- function(x){
    if(!x$taskExecRoleVerified){
        if(!is.null(x$taskExecRoleId)){
            roles <- listRoles()
            if(!x$taskExecRoleId%in%roles$roleArn){
                stop("unable to find the role Arn <", x$taskExecRoleId,">")
            }
        }else{
            if(x$logDriver == "auto"){
                roles <- listRoles()
                idx <- which(roles$roleName==defaultRoleName)
                if(length(idx)){
                    roleArn <- roles$roleArn[idx]
                }else{
                    roleArn <- createRole()
                }
                policyList <- listPolicies(roleName = defaultRoleName)
                if(!defaultPolicyName%in%policyList){
                    attachDefaultPolicy(roleName = defaultRoleName)
                    Sys.sleep(5)
                }
                x$taskExecRoleId <- roleArn
            }
        }
        x$taskExecRoleVerified <- TRUE
    }
    x$taskExecRoleId
}
