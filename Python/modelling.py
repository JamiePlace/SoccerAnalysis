import pandas as pd
import numpy as np
from utils import seriesgenerator
from utils import cnnlstm
from utils import project_root

def create_data(df, cols):
    df = df[cols]
    for season in np.unique(df.season):
        sub_dat = df[df.season == season].copy()
        team = "Liverpool"

        team_dat = sub_dat[(sub_dat.hometeam == team) | (sub_dat.awayteam == team)].copy()



cols = ["season", "hometeam", "awayteam", "gw",
    "hpos", "apos", "hpoints", "apoints", "hgoalscored",
    "agoalscored", "hgoalconceded", "agoalconceded", "hshots",
    "ashots", "hshotstarget", "ashotstarget", "hfouls",
    "afouls", "hcorners", "acorners", "hgw", "agw"]

# get root of project
root = project_root()
# load in the data
data = pd.read_csv(root + "data\\csv\\premierleague_data.csv", parse_dates=["timestamp"])

look_back = 6



X_train_seq, y_train_seq = wp.seriesgenerator(X_train, y_train, window = look_back)
X_test_seq, y_test_seq = wp.seriesgenerator(X_test, y_test, window = look_back)


mod = wp.cnnlstm()

mod.create_model((look_back, X_train.shape[1]))
mod.train(X_train_seq, y_train_seq, 10000)