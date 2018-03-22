library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)

maxlen = 20
batch_size = 50
epochs = 100
nChar2Generate = 200
lr = 0.01
n_neurons = 200
data = NA
chars = NA

globalConfigurationLSTM = function (data,
                                     maxlen = 20,
                                     batch_size = 20,
                                     epochs = 100,
                                     nChar2Generate = 200,
                                     lr = 0.01,
                                     n_neurons = 200) {
  
  assign('data', data, envir = .GlobalEnv)
  assign('maxlen', maxlen, envir = .GlobalEnv)
  assign('batch_size', batch_size, envir = .GlobalEnv)
  assign('epochs', epochs, envir = .GlobalEnv)
  assign('nChar2Generate', nChar2Generate, envir = .GlobalEnv)
  assign('lr', lr, envir = .GlobalEnv)
  assign('n_neurons', n_neurons, envir = .GlobalEnv)
}

trainLSTM = function(){

  if(is.na(data)){message("Dataset Empty, configure first");return()}
  
  # ##############################################################################
  
  data = data %>%
    str_c(collapse = "\n") %>%
    tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE, lowercase = FALSE)
  
  # ##############################################################################
  
  
  print(sprintf("corpus length: %d", length(data)))
  
  chars = data %>%
    unique() %>%
    sort()
  
  print(sprintf("total chars: %d", length(chars)))  
  
  # Cut the data in semi-redundant sequences of maxlen characters
  dataset = map(
    seq(1, length(data) - maxlen - 1, by = 3), 
    ~list(sentece = data[.x:(.x + maxlen - 1)], next_char = data[.x + maxlen])
  )
  
  dataset = transpose(dataset)
  
  # Vectorization
  X = array(0, dim = c(length(dataset$sentece), maxlen, length(chars)))
  y = array(0, dim = c(length(dataset$sentece), length(chars)))
  
  for(i in 1:length(dataset$sentece)){
    
    X[i,,] = sapply(chars, function(x){
      as.integer(x == dataset$sentece[[i]])
    })
    
    y[i,] = as.integer(chars == dataset$next_char[[i]])
    
  }
  
  assign('data', data, envir = .GlobalEnv)
  assign('chars', chars, envir = .GlobalEnv)
  
  # Model Definition --------------------------------------------------------
  
  model = keras_model_sequential()
  
  model %>%
    layer_lstm(n_neurons, input_shape = c(maxlen, length(chars))) %>%
    layer_dense(length(chars)) %>%
    layer_activation("softmax")
  
  optimizer = optimizer_rmsprop(lr = lr, decay = 0.0001)
  
  model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = optimizer
  )
  
  # Training  ----------------------------------------------------

  model %>% fit(X, y, batch_size = batch_size, epochs = epochs)

  return(model)
}

predictLSTM = function(model,diversity){
  
  sample_mod = function(preds, temperature = 1){
    preds = log(preds)/temperature
    exp_preds = exp(preds)
    preds = exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
      as.integer() %>%
      which.max()
  }
  
  cleanNum = function(generated){
    
    cleanGenerated <- strsplit(generated, "")[[1]]
    clean = cleanGenerated[1]
    
    for (i in 2:length(cleanGenerated)) {
      if (is.na(as.numeric(cleanGenerated[i]))){
        clean = paste0(clean,cleanGenerated[i])
      } else if(is.na(as.numeric(cleanGenerated[i-1]))){
        clean = paste0(clean,cleanGenerated[i])
      }
    }
    return(clean)
  }
  
    
  cat(sprintf("diversity: %f ---------------\n\n", diversity))
  
  start_index = sample(1:(length(data) - maxlen), size = 1)
  sentence = data[start_index:(start_index + maxlen - 1)]
  generated = ""
  
  for(i in 1:nChar2Generate){
    
    x = sapply(chars, function(x){ as.integer(x == sentence) })
    x = array_reshape(x, c(1, dim(x)))
    
    preds = predict(model, x)
    next_index = sample_mod(preds, 1.5)
    next_char = chars[next_index]
    
    generated = str_c(generated, next_char, collapse = "")
    sentence = c(sentence[-1], next_char)
    
  }
  
  generated = cleanNum(generated)
  cat(generated,"\n")
  return(generated)
}

