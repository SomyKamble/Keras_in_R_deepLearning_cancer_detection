library(mlbench)
library(keras)

data2<-data(BreastCancer)
data2<-BreastCancer
data2<-na.omit(data2)
data2<-data2[-1]
data2<-as.matrix(data2)

data2[,10]<-as.numeric(as.factor(data2[,10]))

split<-sample(1:2,nrow(data2),prob = c(0.8,0.2),replace = T)

train<-data2[split==1,]

test<-data2[split==2,]

trainx<-train[,1:9]
train_y<-as.numeric(as.factor(train[,10]))

trian_y<-train_y-1


y_train<- to_categorical(trian_y)


testx<-test[,1:9]
test_y<-as.numeric(as.factor(test[,10]))
test_y<-test_y-1
y_test<-to_categorical(test_y)


model<-keras_model_sequential()

model %>%
  layer_dense(units =512,activation = 'relu',input_shape = 9) %>%
  layer_dense(units = 10, activation = 'relu')%>%
  layer_dense(units = 10, activation = 'relu')%>%
  
  layer_dense(units = 2, activation = 'sigmoid')
  


model %>% compile(
  loss='binary_crossentropy',
  optimizer='adam',
  metrics=c('accuracy')
)

summary(model)
trainx<-as.matrix(trainx)
class(trainx)
history <- model %>% fit(trainx,y_train,epochs=50,batch_size=5,validation_split=0.2)

plot(history)

score <- model %>% evaluate(testx, y_test)

cat('Test loss:', score$loss, "\n")
cat('Test accuracy:', score$acc, "\n")

sam<-predict_classes(model,testx)


library(caret)

table(sam,test_y)

85+54
