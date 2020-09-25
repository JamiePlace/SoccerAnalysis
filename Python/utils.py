import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import regularizers
from tensorflow.keras import optimizers
from tensorflow.keras import backend as K
from tensorflow.keras.models import Model, Sequential
from tensorflow.keras.layers import Input, Concatenate, LSTM, Dense, Conv1D, Dropout, MaxPooling1D, AveragePooling1D, Softmax
from sklearn.model_selection import train_test_split
import datetime
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import plotly.graph_objects as go

# this function lists the available devices
def listdevices():
    print(tf.config.list_physical_devices())

# this function creates the input data for the model
def seriesgenerator(X, y, window = 1):
    origlen = len(y)
    modlen  = origlen % window
    
    # remove starting data points in order to make dataset line up
    X = X[modlen : origlen, :]
    y = y[modlen : origlen]
    
    # define how many series we will produce
    numseries = int(len(y) - window)

    
    # numpy [rows, columns]
    # make empty data and fill later
    outputX  = np.zeros((numseries, window, X.shape[1]))
    outputy = np.zeros((numseries))
    
    for i in range(numseries):
        outputX[i, :, :] = X[i : i + window, :]
        outputy[i]       = y[i + int(window)] 
    
    return outputX, outputy

# class that defines all aspects of the model
class cnnlstm():
    # on initialisation we define:
    #
    #  - batch size
    #  - number of lstm nodes
    #  - number of cnn filters
    #
    # this could be hardcoded as the values rarely change
    # however this adds robustness.
    def __init__(self, batch = 256, nodes = 64, filters = 32):
        self.batch = batch
        self.nodes = nodes
        self.filters = filters
    
    # this is a function to create he model
    def create_model(self, input_shape, output_type = "regression", num_class = None):
        # the model uses the keras functional API
        # this allows for very easy manipulation of the model architecure while
        # maintaining readability
        # it also looks more tidy in my oppinion.

        # this funciton returns a convolution layer
        # often there is only one layer however we provide robustness
        # and tidyness by allowing the user to add via a single call
        def conv_layer(filters, k, name, channel = "channels_last"):
            return Conv1D(filters = filters, kernel_size = k, padding = "causal", data_format=channel, activation  = 'relu', kernel_initializer="TruncatedNormal", name = name)

        # this is an optimiser that uses adam with momentum
        # this ended up not being used, but I will leave this line in
        # so it could be used in future.
        adam = optimizers.Nadam(learning_rate=0.01, beta_1=0.9, beta_2=0.999)
        
        # not sure where to define the model being used on a GPU
        # will use this "with" statement through out
        with tf.device('/GPU:0'):
            # defines the inputs to the model. input shape has been defined when we call the method to create the model
            inp_node = Input(shape = input_shape, name = "Inputs")
            ############## model ###############
            # kernel size is number of features
            # this is used to find "spatial" relations between all features
            # a larger kernel size would be impossible
            # a smaller kernel size doesn't make sense as the first layer. Could be added after a pooling layer if you wished.
            conv_err2 = conv_layer(self.filters, input_shape[1], "Convolution_error2")(inp_node)

            # pooling layer to find max activation of each filter
            pool_error1  = MaxPooling1D(name = "Pool_error1")(conv_err2)

            # lstm layer to find temporal relations between the spatial relations defined above by the cnn
            lstm_error1  = LSTM(self.nodes, name = "LSTM_error_1")(pool_error1)

            if self.nodes > 64:
                # several dense layers that step down to one output
                dense_error1 = Dense(64, activation = "relu", name = "dense_error_1")(lstm_error1)
                dense_error2 = Dense(32, activation = "relu", name = "dense_error_2")(dense_error1)
                dense_error3 = Dense(16, activation = "relu", name = "dense_error_3")(dense_error2)
                if output_type == "regression":
                    dense_error5 = Dense(1, name = "dense_error_5")(dense_error3)
                if output_type == "classification":
                    dense_error5 = Dense(num_class, activation = "softmax", name = "dense_error_5")(dense_error3)

            # method of automatically halving the nodes from the output from the lstm
            # to try and limit potential overfit for smaller networks
            else:
                dense_error1 = Dense(int(self.nodes/2), activation = "relu", name = "dense_error_1")(lstm_error1)
                if output_type == "regression":
                    dense_error5 = Dense(1, name = "dense_error_5")(dense_error1)
                if output_type == "classification":
                    dense_error5 = Dense(num_class, activation = "softmax", name = "dense_error_5")(dense_error1)


            ######## define inputs and outputs #########
            err_mod = Model(inputs = [inp_node], outputs = [dense_error5])
            # compile models
            if output_type == "regression":
                err_mod.compile(
                    optimizer = 'adam',
                    loss = 'mse'
                )
            if output_type == "classification":
                err_mod.compile(
                    optimizer = 'adam',
                    loss = 'categorical_crossentropy'
                )
            print(err_mod.summary())
            self.model = err_mod

    # function used to train the model
    # taking in features, target, number of epochs
    # validation data can be added however I prefer to use a proportion of the training data
    def train(self, X, y, num_epochs, valid_x = None, valid_y = None):
        
        # can use tensorboard if installed
        # never worked for me, seemed like it would be cool... oh well
        #log_dir="logs\\fit\\" + datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        #tensorboard_callback = tf.keras.callbacks.TensorBoard(log_dir=log_dir, histogram_freq=1)

        # defining validation data based on sklearn splits. Keras automatically uses the last portion of the data
        # this is not entirely what we want
        X_train, X_valid, y_train, y_valid = train_test_split(X, y, test_size = 0.1, shuffle = True)

        # two different operations if batch size is or is not defined
        # recomend using batch size as training would be very slow otherwise
        if self.batch is None:
            with tf.device('/GPU:0'):
                self.history = self.model.fit(
                    X_train,
                    y_train,
                    validation_data=(X_valid, y_valid),
                    epochs = num_epochs,
                    shuffle = True,
                    #callbacks=[tensorboard_callback],
                    verbose = 1,
                )
        else:
            with tf.device('/GPU:0'):
                self.history = self.model.fit(
                    X_train,
                    y_train,
                    validation_data=(X_valid, y_valid),
                    shuffle = True,
                    epochs = num_epochs,
                    batch_size = self.batch,
                    #callbacks=[tensorboard_callback],
                    verbose = 1
                )
    
    # this function outputs the model if needed
    # ofcourse one could simply call the model directly from the class
    # but having it explicit somewhere in the code is nice and clear
    def getmodel(self):
        return self.model
    
    # plot the training history
    # allowing the user to plot from a certain epoch
    # convenient when the initial improvements are large
    def plothistory(self, start = 0):
        plt.figure(figsize = (16,12))
        plt.plot(self.history.history["loss"][start :])
        plt.plot(self.history.history["val_loss"][start :])

# class that enables the user to examine the performance of
# a given model using repeated n-fold cross validation
# leveraging the power of the central limit theorem
# 
class crossvalidate():

    def __init__(self):
        return None

    # function to compute mean squared error
    def mse_score(self, y, y_):
        score = np.mean(np.square(y - y_))
        return(score)
    # function to compute the mean absolute error
    def mae_score(self, y, y_):
        score = np.mean(np.abs(y - y_))
        return(score)
    # function to compute the root mean squared error
    def rmse_score(self, y, y_):
        score = np.sqrt(np.mean(np.square(y - y_)))
        return(score)
    # function to perform repeated n-fold crossvalidation
    # n = 50
    # performance is not our goal, precision is more important
    def validate(self, df, predcolumn = "Prediction", actualcolumn = "Actual", nruns = 1000, metric = "mse"):
        self.predcolumn = predcolumn
        self.actualcolumn = actualcolumn

        nrow = len(df.index) # number of rows of data
        nfolds = 50 # number of folds
        
        nrowsperfold = nrow // nfolds # how many rows per fold need to be extracted

        self.score = [] # list to store the results of our error metric per fold

        for itter in range(nruns):
            df = df.sample(frac=1).reset_index(drop=True)
            folddata = []
            for i in range(nfolds):
                startidx = 0 + i*nrowsperfold
                # condition, if we are at the end of our loop
                # go to the end of the dataframe,
                # isn't necesarily the same amount of rows
                if nrow % nfolds != 0 and i == nfolds - 1:
                    endidx = i*nrowsperfold + (nrow % nfolds) - 1
                else:
                    endidx   = nrowsperfold + i*nrowsperfold

                # append the sectionf of the dataframe for testing to the list
                folddata.append(df.iloc[startidx : endidx])
            # for each section of dataframe evaluate the error
            for fold in folddata:
                y = fold[self.actualcolumn]
                y_ = fold[self.predcolumn]
                # mean squared error
                if metric == "mse":
                    self.score.append(self.mse_score(y, y_))
                # mean absolute error
                if metric == "mae":
                    self.score.append(self.mae_score(y, y_))
                # root mean sqaured error
                if metric == "rmse":
                    self.score.append(self.rmse_score(y, y_))
        
        self.score = np.asarray(self.score, dtype=np.float32)
        # empirical mean and variance computed
        self.mu = np.mean(self.score)
        self.var = np.var(self.score)

        self.score = self.score[(self.score > self.mu - (3 * (self.var ** 0.5))) & (self.score < self.mu + (3 * (self.var ** 0.5)))]
        self.mu = np.mean(self.score)
        self.var = np.var(self.score)
        # due to central limit theorem we can easily calculate the 95 % confidence value
        self.perc_95 = 1.96 * (self.var ** 0.5) # 95% confidence level

        return self.mu, self.perc_95
    
    def plot(self, filepath = None):
        # normal curve based on values that will create the histogram
        pdf_x = np.linspace(np.min(self.score),np.max(self.score),100)
        pdf_y = 1.0/np.sqrt(2*np.pi*self.var)*np.exp(-0.5*(pdf_x-self.mu)**2/self.var)

        # upper and lower values based on 95% confidence interval
        upr = self.mu + self.perc_95
        lwr = self.mu - self.perc_95

        # text to be shown in the plot
        text_x = upr + 0.5*(self.var ** 0.5)
        text_y = max(pdf_y)
        text_string = r"Error = {:.3} $\pm$ {:.3}".format(self.mu, self.perc_95)

        plt.figure(figsize = (16,12))
        plt.hist(self.score, density=True, bins = 50, label="Error Value Count", rwidth=0.85)
        plt.plot(pdf_x, pdf_y, 'r--', linewidth=2, label="Normal PDF")
        plt.axvline(x = upr, linestyle = '--', color = 'black', label = "95% Confidence Level")
        plt.axvline(x = lwr, linestyle = '--', color = 'black')
        plt.text(text_x, text_y, text_string)
        plt.title("Error distribution via repeated n-fold crossvalidation")
        plt.legend(loc="upper left")
        # make y axis ticks invisible as they mean nothing
        plt.yticks([], [])
        # labels for axis
        plt.xlabel("Error Value")
        plt.ylabel("Density")

        if filepath is not None:
            plt.savefig(filepath, bbox_inches='tight')    