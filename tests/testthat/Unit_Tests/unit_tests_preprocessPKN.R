##This function runs a unit test for the function preprocessPriorKnowledgeNetwork
#
# Matteo Spatuzzi 2021


# Create Dummy Network

nodenames = c(paste("Node", seq(1,5), sep = ""))

Dummy_priorKnowledgeNetwork_1 = matrix(c(nodenames[1], 1, nodenames[4], 
                                   nodenames[2], 1, nodenames[4],
                                   nodenames[3], -1, nodenames[1],
                                   nodenames[3], -1, nodenames[2],
                                   nodenames[4], 1, nodenames[5]), byrow = T, ncol = 3) %>% as.data.frame()

Expected_priorKnowledgeNetwork_1 = Dummy_priorKnowledgeNetwork_1

colnames(Dummy_priorKnowledgeNetwork_1) = c("source", "interaction", "target")
colnames(Expected_priorKnowledgeNetwork_1) = c("Node1", "interaction", "target")

preprocessPriorKnowledgeNetwork(Dummy_priorKnowledgeNetwork_1)
