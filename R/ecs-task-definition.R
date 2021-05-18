CreateTaskDefinition <- function(taskDefName, name, image,
                                 logConfiguration = NULL,
                                 executionRoleArn = NULL,
                                 cpu = 256, memory = 512, ...){
    if(is.null(name)){
        name <- "container"
    }
    containerDefinitions <- list(list(
        name = name,
        image=image,
        essential = TRUE
    ))
    containerDefinitions[[1]]$logConfiguration <- logConfiguration
    response <- ecs_register_task_definition(family=taskDefName,
                                             cpu = as.character(256),
                                             memory = as.character(memory),
                                             containerDefinitions = containerDefinitions,
                                             networkMode = "awsvpc",
                                             requiresCompatibilities = "FARGATE",
                                             executionRoleArn  = executionRoleArn,
                                             ...
    )
    response$taskDefinitionArn
}

deleteTaskDefinition <- function(taskName, version = NULL, ...){
    if(!is.null(version)){
        taskName <- paste0(taskName,":", version)
    }
    response <- ecs_deregister_task_definition(taskDefinition = taskName, ...)
    response
}

describeTaskDefinition <- function(taskName, version = NULL, ...){
    if(!is.null(version)){
        taskName <- paste0(taskName,":", version)
    }
    response <- ecs_describe_task_definition(taskDefinition = taskName, ...)
    response
}

listTaskDefinitions<-function(taskName = NULL, ...){
    defArns <- ecs_list_task_definitions(familyPrefix = taskName, ...)
    defInfo <- ECSGetResourceNames(defArns)
    defInfo <- strsplit(defInfo,":", fixed = TRUE)
    defNames <- vapply(defInfo, function(x)x[1], character(1))
    defVersions <- vapply(defInfo, function(x)as.numeric(x[2]),numeric(1))
    data.frame(name = defNames, version = defVersions)
}

configTaskDefinition <- function(x, cluster, ...){
    if(!is.null(.getServerContainer(cluster))){
        configTaskDefinitionInternal(x,
                                     "serverTaskDefName",
                                     .getServerContainer(cluster),
                                     ...)
    }
    configTaskDefinitionInternal(x,
                                 "workerTaskDefName",
                                 .getWorkerContainer(cluster),
                                 ...)

}

configTaskDefinitionInternal <- function(x, taskNameSlot, container, ...){
    taskDefName <- x$field(taskNameSlot)
    verifySlot <- paste0(taskNameSlot, "Verified")
    taskDefNameVerified <- x$field(verifySlot)
    if(!taskDefNameVerified){
        definitionList <- listTaskDefinitions(taskName = taskDefName, ...)
        needDef <- nrow(definitionList)==0
        if(!needDef){
            idx <- which.max(definitionList$version)
            taskInfo <- describeTaskDefinition(taskDefName, definitionList$version[idx],
                                               ...)

            containerDef <- taskInfo$containerDefinitions[[1]]
            nameCheck <- identical(
                containerDef$name,
                container$name
            )
            imageCheck <- identical(
                containerDef$image,
                container$image)

            logConfig <- getLogJson(x,taskDefName)
            logCheck <- identical(
                containerDef$logConfiguration$logDriver,logConfig$logDriver)&&
                listSetEqual(containerDef$logConfiguration$options,
                             logConfig$options)

            needDef <- any(!nameCheck, !imageCheck, !logCheck)
        }

        if(needDef){
            CreateTaskDefinition(
                taskDefName = taskDefName,
                name = container$name,
                image =  container$image,
                logConfiguration=logConfig,
                executionRoleArn=x$taskExecRoleId,
                ...
            )
        }
        x$field(verifySlot, TRUE)
    }
}

