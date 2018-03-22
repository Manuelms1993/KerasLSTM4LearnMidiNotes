# Manuel Montero

# Configuration working directories #

projectPath = "/home/user/LOL"
setwd(projectPath)
modelName = "test"
pathIn = paste0(projectPath,"/Data/",modelName,"/Midis")
pathData = paste0(projectPath,"/Data/",modelName,"/RDS")
pathOut = paste0(projectPath,"/Data/",modelName,"/Melodies")

# Libraries #

source(paste0(projectPath,"/Bin/Persistence/RWoperations.R"))
source(paste0(projectPath,"/Bin/NeuralNetworks/LSTM.R"))

# Creating abcNotation rds file and set working directory

midi2abc(pathIn, pathData, name = paste0(modelName,".rds"))
setwd(projectPath)

# load data and train it

text = loadRDS(file.path(pathData,paste0(modelName,".rds")))
globalConfigurationLSTM(data = text, maxlen = 50, epochs = 250, lr=0.001, n_neurons = 100 , batch_size = 10, nChar2Generate = 1000)
model = trainLSTM() 

# genereating new melodies

predict = predictLSTM(model, diversity = 1)

