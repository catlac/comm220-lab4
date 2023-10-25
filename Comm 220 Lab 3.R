install.packages('igraph')
install.packages('scales')
library(igraph)
library(scales)
source('functions.R')

# //////////////////////////////////////////////////////
# here are our model parameters...
# //////////////////////////////////////////////////////

n_agents = 30 
influence_rate = .1 # information "impulse" between agents
have_info = n_agents*.5 # how many agents start with the information (=1)
connectedness = 1 # proportion network that stays connected
rounds_of_interaction = n_agents*30 # number of pairwise "conversations"

#set.seed(11) # set seed to make code reproducible!

# //////////////////////////////////////////////////////
# initialize agents, connections, and run
# //////////////////////////////////////////////////////

agent_states = rep(0,n_agents) # initialize n_agents to 0 (rep = repeat)
agent_states[sample(1:n_agents,have_info)] = 1 # randomly choose who starts with info

n_delete = round((1-connectedness)*n_agents^2) # # connections to zero out
connection_matrix = make_connections(n_agents,n_delete=n_delete,type='circle')
# BLOCK 1 //////////////////////////////////////////////
#connection_matrix[1,16] = 1 # make a long range (a 'hub') 
#connection_matrix[15,4] = 1 # make a long range (a 'hub')
# //////////////////////////////////////////////////////
edge_list = which(connection_matrix>0,arr.ind=TRUE)

history = run_model(agent_states=agent_states,
                    edge_list=edge_list,
                    rounds_of_interaction=rounds_of_interaction,
                    influence_rate=influence_rate)

# //////////////////////////////////////////////////////
# let's do some plotting of history and average agent states
# //////////////////////////////////////////////////////

colors = alpha(rainbow(n_agents),.25)
par(mfrow=c(2,2))

plot_graph(connection_matrix,history[1,],colors) 
plot_graph(connection_matrix,history[nrow(history),],colors) 

plot_history(history,colors=colors,line_width=3,
             line_type='l',y_axis_range=c(0,1))
plot(rowMeans(history),type='l',ylim=c(0,1),
     xlab='Iteration',ylab='Average state')


############

# NOTE: this was a comparison lab, so this is the same script as above with different influence_rate and have_info values.

n_agents = 30 
influence_rate = .5 # information "impulse" between agents
have_info = n_agents*.1 # how many agents start with the information (=1)
connectedness = 1 # proportion network that stays connected
rounds_of_interaction = n_agents*30 # number of pairwise "conversations"

#set.seed(11) # set seed to make code reproducible!

# //////////////////////////////////////////////////////
# initialize agents, connections, and run
# //////////////////////////////////////////////////////

agent_states = rep(0,n_agents) # initialize n_agents to 0 (rep = repeat)
agent_states[sample(1:n_agents,have_info)] = 1 # randomly choose who starts with info

n_delete = round((1-connectedness)*n_agents^2) # # connections to zero out
connection_matrix = make_connections(n_agents,n_delete=n_delete,type='circle')
# BLOCK 1 //////////////////////////////////////////////
#connection_matrix[1,16] = 1 # make a long range (a 'hub') 
#connection_matrix[15,4] = 1 # make a long range (a 'hub')
# //////////////////////////////////////////////////////
edge_list = which(connection_matrix>0,arr.ind=TRUE)

history = run_model(agent_states=agent_states,
                    edge_list=edge_list,
                    rounds_of_interaction=rounds_of_interaction,
                    influence_rate=influence_rate)

# //////////////////////////////////////////////////////
# let's do some plotting of history and average agent states
# //////////////////////////////////////////////////////

colors = alpha(rainbow(n_agents),.25)
par(mfrow=c(2,2))

plot_graph(connection_matrix,history[1,],colors) 
plot_graph(connection_matrix,history[nrow(history),],colors) 

plot_history(history,colors=colors,line_width=3,
             line_type='l',y_axis_range=c(0,1))
plot(rowMeans(history),type='l',ylim=c(0,1),
     xlab='Iteration',ylab='Average state')
