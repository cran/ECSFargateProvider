context("Testing the provider")

# We will enable this test after the container package has been submitted to CRAN
# if(existCredentials()){
#     aws.ecx::aws_set_retry_time(20)
#     aws.ecx::aws_set_network_timeout(5)
#     workerPerContainer <- 2L
#     provider <- ECSFargateProvider()
#     container <-foreachRedisContainer::doRedisWorkerContainer()
#     container$maxWorkerNum <- workerPerContainer
#     generalDockerClusterTest(cloudProvider = provider, workerContainer = container)
# }


test_that("Testing the constructor function", {
    expect_error(provider <- ECSFargateProvider(), NA)
})
