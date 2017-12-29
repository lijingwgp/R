library(optrees)
nodes <- seq(1,8)
arcs <- matrix(c(1,2,20, 1,3,15, 2,4,10, 2,5,15, 3,4,13, 3,6,15, 3,7,10, 4,3,13,
                 4,5,10, 4,7,12, 5,2,15, 5,6,7, 5,8,10, 6,5,7, 6,7,8, 
                 6,8,8, 7,6,8, 7,8,10), byrow = TRUE, ncol = 3)
max_flow = maxFlowFordFulkerson(nodes = nodes,arcs = arcs,directed = T,source.node = 1,sink.node = 8)
max_flow$max.flow


# We want to increase 5% flow in and flow out capacity each time on top of the original capacity
# The while loop ensures us to get the first max flow value that is greater than 56
# After the while loop is done, the most updated value of k indicates the least upper bond
k = 1.05
while(max_flow$max.flow <= 56){
  arcs <- matrix(c(1,2,20, 1,3,15, 2,4,10, 2,5,15, 3,4,13, 3,6,15, 3,7,10, 4,3,13,
                   4,5,10, 4,7,12, 5,2,15, 5,6,7, 5,8,10, 6,5,7, 6,7,8, 
                   6,8,8, 7,6,8, 7,8,10), byrow = TRUE, ncol = 3)
  arcs[c(1,2,13,16,18),3] = arcs[c(1,2,13,16,18),3]*k
  max_flow = maxFlowFordFulkerson(nodes = nodes,arcs = arcs,directed = T,source.node = 1,sink.node = 8)
  if(max_flow$max.flow < 56){
    k = k+0.05
  }
}
max_flow$max.flow


# Now, our job is to improve the accuracy of k. The goal is the get an estimate of k
# that will increase network's max flow that is as close to 56 as possible
while((max_flow$max.flow-56)>=0){
  k=k-0.00055
  arcs <- matrix(c(1,2,20, 1,3,15, 2,4,10, 2,5,15, 3,4,13, 3,6,15, 3,7,10, 4,3,13,
                   4,5,10, 4,7,12, 5,2,15, 5,6,7, 5,8,10, 6,5,7, 6,7,8, 
                   6,8,8, 7,6,8, 7,8,10), byrow = TRUE, ncol = 3)
  arcs[c(1,2,13,16,18),3] = arcs[c(1,2,13,16,18),3]*k
  max_flow = maxFlowFordFulkerson(nodes = nodes,arcs = arcs,directed = T,source.node = 1,sink.node = 8)
}
max_flow$max.flow
print("It is possible to increase the maximum flow to 63 which surpassed initial expectation. In fact, no matter how we increase the capacities of leading out and leading in pipelines, the largest amount of maximum flow is 63")
print("That being said, it is not possible to increase the capacity of all arcs leading out of tank1 and leading into tank8 by a factor k would allow the maximum flow to be doubled exactly.")
print("However, it is possible to increase the maximum flow to a value that is as close to double as possible")
Print("In fact, this number is approximatly 52.9998 which is the largest maximum flow we can achieve before passing 56")
print("There is a limit to increasing the expansion factor k. This limit is approximatly 1.8925")
print("The value of k indicates that we can at most increase the original flow in and flow out capacity by 189.3%, before the max flow of the network pass 56")



