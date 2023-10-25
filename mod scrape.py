#### Setup ####

import time
from datetime import datetime
import json
import math
import os
import pathlib
import re
from collections import Counter
import itertools
import random
import ast
import praw
from prawcore.exceptions import Forbidden, NotFound


#### Setup Fcns ####

start = time.time()

def round_down(n, decimals=0):
    multiplier = 10 ** decimals
    return math.floor(n * multiplier) / multiplier

def time_elapsed():
    elapsed = time.time() - start
    minutes = round_down(elapsed / 60)
    seconds = elapsed - minutes * 60
    time_elapsed = str(round(minutes)) + " m, " + str(round(seconds)) + " s"
    return time_elapsed

def save(file_location, mod_set):
    with open(file_location, "w", encoding = "utf-8") as file:
        for sub in mod_set.keys():
            file.write(sub)
            file.write("\t")
            for mods in mod_set[sub]:
                file.write(mods)
                file.write("\t")
            file.write("\n")


#### Finding Mods ####

sub_set = ["r/GeorgeFloydRevolution", "r/DenverProtests", "r/policebrutality",
           "r/RepublicanValues", "r/dsa", "r/leftistvexillology", "r/thedavidpakmanshow",
           "r/racism", "r/VaushV", "r/asianamerican", "r/Trumpvirus", "r/ACAB",
           "r/WayOfTheBern", "r/SocialistRA", "r/AntifascistsofReddit", "r/BlackLivesMatter",
           "r/Anarchy101", "r/esist", "r/MarchAgainstNazis", "r/Political_Revolution",
           "r/DemocraticSocialism", "r/AOC", "r/COMPLETEANARCHY", "r/BreadTube",
           "r/ShitLiberalsSay", "r/Fuckthealtright", "r/DankLeft", "r/ENLIGHTENEDCENTRISM",
           "r/punk", "r/communism", "r/FragileWhiteRedditor", "r/Anarchism", "r/lostgeneration",
           "r/socialism", "r/ToiletPaperUSA", "r/collapse", "r/SandersForPresident",
           "r/Bad_Cop_No_Donut", "r/WitchesVsPatriarchy", "r/ABoringDystopia",
           "r/LateStageCapitalism", "r/WhitePeopleTwitter", "r/BlackPeopleTwitter", "r/TwoXChromosomes",
           "r/askaconservative", "r/AskThe_Donald", "r/AskTrumpSupporters",
           #"r/Catholicism",
           "r/CoincidenceTheorist", "r/Conservative", "r/ConservativeMemes", "r/conservatives",
           "r/ConservativesOnly", "r/DrainTheSwamp", "r/HillaryForPrison",
           # "r/JordanPeterson",
           "r/kotakuinaction2", "r/LouderWithCrowder", "r/NEWPOLITIC", "r/ProtectAndServe",
           "r/Republican", "r/ShitPoliticsSays", "r/TimPool", "r/TruthLeaks", "r/tucker_carlson", "r/walkaway"
            ]

reddit = praw.Reddit(
    client_id="iRyyoitfiZVHAbXztHbRNw",
    client_secret="******************",
    password="***********",
    user_agent="scrape by u/Penn_Researcher",
    username="Penn_Researcher",
)

def scrape_mods(subreddits):
    mod_set = {}
    for sub in subreddits:
        mods = []
        sub = sub.split("/")[1]
        #print(sub)
        try:
            for moderator in reddit.subreddit(sub).moderator():
                mod = moderator.name
                mods.append(mod)
            mod_set[sub] = mods
        except Forbidden: mod_set[sub] = ["private"]
        except NotFound: mod_set[sub] = ["banhammered"]
        #print(mod_set)
    return mod_set


#### Main ####

def main():
    mod_set = scrape_mods(sub_set)
    save("C:/Users/brend/Documents/diss/methods/sampling/mod_set.txt", mod_set)

main()