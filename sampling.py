#### Setup ####

import time
from datetime import datetime
import json
import math
import os
import re
from nltk import SnowballStemmer
from file_read_backwards import FileReadBackwards
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from webdriver_manager.chrome import ChromeDriverManager


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

def save_json(file_location, posts):
    with open(file_location, "w", encoding = "utf-8") as file:
        for post in posts:
            json.dump(post, file)
            file.write("\n")

def save(file_location, posts):
    with open(file_location, "w", encoding = "utf-8") as file:
        for post in posts:
            for p in post:
                #print(p)
                if type(p) == list:
                    for puh in p:
                        file.write(str(puh))
                        file.write("\t")
                else:
                    file.write(str(p))
                    file.write("\t")
            file.write("\n")

def correct_url(url):
    if not url.startswith("http://") and not url.startswith("https://"):
        url = "http://" + url
    return url

def scrollDown(browser, numberOfScrollDowns):
    body = browser.find_element_by_tag_name("body")
    while numberOfScrollDowns >= 0:
        body.send_keys(Keys.PAGE_DOWN)
        numberOfScrollDowns -= 1
    return browser


#### Build Keyword List ####

keyword_list = ["George Floyd", "Floyd", "Derek Chauvin", "Chauvin", "Tou Thao", "Thomas Lane",
                "J. Alexander Kueng", "Breonna Taylor", "Tony McDade", "Black Lives Matter",
                "BLM", "antifa", "anarchists", "anarchy", "Boogaloo Boi", "Boogaloo Boy",
                "Capitol Hill Autonomous Zone", "CHAZ", "chokeholds", "qualified immunity",
                "systemic racism", "racism", "police misconduct", "police brutality",
                "defund the police", "abolish the police", "say his name", "say her name",
                "I can’t breathe", "Blue Lives Matter", "All Lives Matter",
                "Ahmaud Arbery", "vandalism", "riot", "rioter", "looter",
                "police union", "anti-police", "David Dorn", "disband the police",
                "autonomous zone", "tear gas", "police violence", "racial justice"
                ]
s = SnowballStemmer("english")

def clean_list(list):
    for i in range(0, len(list)):
        key = list[i].split(" ")
        #print(key)
        clean_key = ""
        for k in key:
            k = s.stem(re.sub(r"\\n|[^\w ]", "", k))
            #print(k)
            clean_key += k + " "
        list[i] = clean_key[:-1]
    return list
keyword_list = clean_list(keyword_list)


#### Build Keyword_set ####

def build_keyword_set(keyword_list):

    keyword_set = []

    with FileReadBackwards("E:/reddit/submissions/RS_2020-05", encoding = "utf-8") as f:
        for line in f:
            content = json.loads(line)
            date = datetime.utcfromtimestamp(content["created_utc"]).strftime('%Y-%m-%d')
            #print(date)
            #print(content)
            if date > "2020-05-25":
                post = str(content["title"] + " " + content["selftext"])
                post_stem = [s.stem(re.sub(r"\\n|[^\w ]", "", word)) for word in post.split(" ")]
                post_stem = " ".join(post_stem)
                #print(post)
                if any(keyword in post_stem for keyword in keyword_list) is True:
                    #print(post)
                    keyword_set.append(content)
                    #print(keyword_set)
            else:
                f.close()

    print("\tmay finished in:", time_elapsed())

    with open("E:/reddit/submissions/RS_2020-06", "r", encoding = "utf-8", errors = "ignore") as f:
        for line in f:
            content = json.loads(line)
            post = str(content["title"] + " " + content["selftext"])
            post_stem = [s.stem(re.sub(r"\\n|[^\w ]", "", word)) for word in post.split(" ")]
            post_stem = " ".join(post_stem)
            #print(post)
            if any(keyword in post_stem for keyword in keyword_list) is True:
                #print(post)
                keyword_set.append(content)

    print("\tjune finished in:", time_elapsed())

    #print(keyword_set)
    return keyword_set

# def merge_keywords(set1, set2):
#     dict = {}
#     with open(set1, "r", encoding = "utf-8", errors = "ignore") as f:
#         for line in f:
#             content = json.loads(line)
#             #print(content)
#             link = content["permalink"]
#             dict[link] = content
#     with open(set2, "r", encoding = "utf-8", errors = "ignore") as f:
#         for line in f:
#             content = json.loads(line)
#             link = content["permalink"]
#             if link in dict.keys():
#                 pass
#             else: dict[link] = content
#     full_set = dict.values()
#     return full_set


#### Build Subreddit_set ####

def build_subreddit_set(keyword_set, mentions):
    subreddit_set = {}
    threshold = []
    with open(keyword_set, "r", encoding = "utf-8", errors = "ignore") as f:
        for line in f:
            content = json.loads(line)
            #print(content)
            subreddit = "r/" + str(content["subreddit"])
            #print(subreddit)
            if subreddit not in subreddit_set:
                subreddit_set[subreddit] = content
            elif type(subreddit_set[subreddit]) == list:
                subreddit_set[subreddit].append(content)
            else: subreddit_set[subreddit] = [subreddit_set[subreddit], content]
    #print(subreddit_set["r/legaladviceofftopic"])
    for sub in subreddit_set.keys():
        # if sub == "r/legaladviceofftopic":
        #     print(sub)
        #     print(subreddit_set[sub])
        #     print(len(subreddit_set[sub]))
        if len(subreddit_set[sub]) >= mentions:
            threshold.append([sub, len(subreddit_set[sub]), subreddit_set[sub]])
    return threshold


#### Retrieve Subreddit Info ####

def get_info(sub):
    desc = "N/A"
    with open("E:/reddit/subreddits/reddit_subreddits.ndjson", "r", encoding = "utf-8") as f:
        for line in f:
            content = json.loads(line)
            if content["display_name_prefixed"] == sub:
                desc = re.sub(r"\s+", " ", content["description"])
                print(desc)
    return desc

def build_codeset(subreddit_set):
    codeset = []
    with open(subreddit_set, "r", encoding="utf-8") as f:
        for line in f:
            line = json.loads(line)
            sub = line[0]
            mentions = str(line[1])
            all_posts = line[2]
            #print(sub)
            #print(mentions)
            #print(all_posts)
            if type(all_posts) == list:
                top_posts = sorted(all_posts, key = lambda k: k["score"], reverse = True)[0:3]
            else: print(sub)
            sample_posts = []
            for i in range(0, 3):
                post = top_posts[i]
                link = "www.reddit.com" + post["permalink"]
                sample_posts.append(link)
            info = get_info(sub)
            row = [sub, mentions, info, sample_posts]
            #print(row)
            codeset.append(row)
    return codeset


#### Sample Posts from Sampled Subs ####

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
           "r/CoincidenceTheorist", "r/Conservative", "r/ConservativeMemes", "r/conservatives",
           "r/ConservativesOnly", "r/DrainTheSwamp", "r/HillaryForPrison",
           "r/kotakuinaction2", "r/LouderWithCrowder", "r/NEWPOLITIC", "r/ProtectAndServe",
           "r/Republican", "r/ShitPoliticsSays", "r/TimPool", "r/TruthLeaks", "r/tucker_carlson", "r/walkaway"
            ]

def build_post_set():
    sub_posts = []
    with open("E:/reddit/submissions/RS_2020-05", encoding="utf-8", errors="ignore") as f:
        for line in f:
            content = json.loads(line)
            subreddit = "r/" + str(content["subreddit"])
            if subreddit in sub_set:
                sub_posts.append(content)
            else: pass
    print("\tmay finished in:", time_elapsed())

    with open("E:/reddit/submissions/RS_2020-06", "r", encoding = "utf-8", errors = "ignore") as f:
        for line in f:
            content = json.loads(line)
            #print(content)
            subreddit = "r/" + str(content["subreddit"])
            #print(subreddit)
            if subreddit in sub_set:
                #print(content)
                sub_posts.append(content)
    print("\tjune finished in:", time_elapsed())

    return sub_posts

def sample_posts(sub_set):
    post_set = {}
    sample_set = []
    with open(sub_set, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            content = json.loads(line)
            # print(content)
            subreddit = "r/" + str(content["subreddit"])
            # print(subreddit)
            if subreddit not in post_set:
                post_set[subreddit] = content
            elif type(post_set[subreddit]) == list:
                post_set[subreddit].append(content)
            else:
                post_set[subreddit] = [post_set[subreddit], content]
                #print(post_set)
    for sub in post_set.keys():
        pre_posts = []
        mid_posts = []
        #print(sub)
        for post in post_set[sub]:
            #print(post)
            date = datetime.utcfromtimestamp(post["created_utc"]).strftime('%Y-%m-%d')
            #print(date)
            if date <= "2020-05-25": pre_posts.append(post)
            else: mid_posts.append(post)
        pre_posts = sorted(pre_posts, key = lambda k: k["score"], reverse = True)
        mid_posts = sorted(mid_posts, key = lambda k: k["score"], reverse = True)
        for i in range(0, 5):
            if len(pre_posts) != 0:
                #print(pre_posts[-1]["score"])
                #p = random.randint(pre_quintiles[i], pre_quintiles[i + 1])
                #print(pre_posts[p]["name"])
                print(pre_posts[i])
                sample_set.append(pre_posts[i])
            else: pass
            if len(mid_posts) != 0:
                #print(mid_posts[-1]["score"])
                #m = random.randint(mid_quintiles[i], mid_quintiles[i + 1])
                print(mid_posts[i])
                sample_set.append(mid_posts[i])
            else: pass
    return sample_set

def extract_comments(sample_set):
    sampled_threads = [["subreddit", "thread id", "date", "score", "depth", "link", "post/comment"]]
    browser = webdriver.Chrome(ChromeDriverManager().install())
    with open(sample_set, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            a = 0
            content = json.loads(line)
            if "url_overridden_by_dest" in content:
                url = content["url_overridden_by_dest"]
            elif content["is_self"] == True:
                url = "selfpost"
            subreddit = "r/" + str(content["subreddit"])
            date = str(datetime.utcfromtimestamp(content["created_utc"]))
            thread_id = content["name"]
            post_score = str(content["score"])
            if "crosspost_parent" in content and content["crosspost_parent_list"]:
                #print(content)
                cross = content["crosspost_parent_list"][0]
                #print(cross)
                submission = re.sub(r"\s+", " ", str("[CROSSPOST] " + content["title"] + ": " + cross["title"] + cross["selftext"]))
                #print(submission)
            else: submission = re.sub(r"\s+", " ", str(content["title"] + ": " + content["selftext"]))
            op = content["author"]
            link = "http://reddit.com" + content["permalink"]
            comments = []
            thread = []
            thread_str = op + ": " + submission + " (url from post: " + url + ")"
            depth = 0
            post = [subreddit, thread_id, date, post_score, depth, link, thread_str]
            sampled_threads.append(post)
            if content["num_comments"] != 0:
                #print(thread_id)
                url = correct_url("https://api.pushshift.io/reddit/comment/search?q=*&link_id=" + thread_id[3:] + "&sort_type=score&size=500")
                while a == 0:
                    try:
                        browser.get(url)
                        file = json.loads(browser.find_element_by_xpath(
                            "//pre[@style = 'word-wrap: break-word; white-space: pre-wrap;']").get_attribute(
                            "innerHTML"))
                        a += 1
                    except TimeoutException:
                        print("takin' a nap (ᴗ˳ᴗ)")
                        time.sleep(5)
                    except NoSuchElementException:
                        print("takin' a nap (ᴗ˳ᴗ)")
                        time.sleep(5)
                comms = file["data"]
                for c in comms:
                    comment_text = re.sub(r"\s+", " ", str(c["body"]))
                    parent = c["parent_id"]
                    id = c["id"]
                    commenter = c["author"]
                    comment_score = c["score"]
                    comment_str = commenter + ": " + comment_text
                    comments.append([id, parent, comment_score, comment_str])
                #print(comments)
                cut = []
                for i in range(0, len(comments)):
                    if comments[i][1] == thread_id:
                        #print(comment)
                        comments[i][3] = "::tab::" + comments[i][3]
                        comments[i].append(1)
                        #print(comments[i])
                        thread.append(comments[i])
                        cut.append(i)
                for c in sorted(cut, reverse = True):
                    #print(c)
                    #print(comments[c])
                    del comments[c]
                    #print(comments)
                #print(thread)
                #print(comments)
                depth = 2
                y = 30
                while y > 0:
                    cut = []
                    tab = "::tab::" * depth
                    for j in range(0, len(comments)):
                        #print(comments[j])
                        for i in range(0, len(thread)):
                            if comments[j][1][3:] == thread[i][0]:
                                #print(comments[j])
                                comments[j][3] = tab + comments[j][3]
                                comments[j].append(depth)
                                #print(comments[j])
                                thread.insert(i+1, comments[j])
                                cut.append(j)
                        #print(thread)
                    #print(cut)
                    for c in sorted(cut, reverse = True):
                        #print(c)
                        #print(comments[c])
                        del comments[c]
                    depth += 1
                    y -= 1
                    #print(thread)
                    #print(comments)
                #print(thread)
                for comment in thread:
                    comment_score = comment[2]
                    depth = comment[4]
                    comment_str = comment[3]
                    comment = [subreddit, thread_id, date, comment_score, depth, link, comment_str]
                    sampled_threads.append(comment)
            time.sleep(3)
            #print(sampled_threads)
    browser.quit()
    return sampled_threads


#### Main ####

def main():
    if os.path.exists("C:/Users/brend/Documents/diss/methods/sampling/keyword_set") is False:
        print("collecting keyword posts...")
        set = build_keyword_set(keyword_list)
        save_json("C:/Users/brend/Documents/diss/methods/sampling/keyword_set", set)
    if os.path.exists("C:/Users/brend/Documents/diss/methods/sampling/subreddit_set.txt") is False:
        print("extracting relevant subreddits...")
        subs = build_subreddit_set("C:/Users/brend/Documents/diss/methods/sampling/keyword_set", 100)
        #print(subs)
        save_json("C:/Users/brend/Documents/diss/methods/sampling/subreddit_set.txt", subs)
    if os.path.exists("C:/Users/brend/Documents/diss/methods/sampling/subreddit_codeset.txt") is False:
        print("building codeset...")
        codeset = build_codeset("C:/Users/brend/Documents/diss/methods/sampling/subreddit_set.txt")
        save("C:/Users/brend/Documents/diss/methods/sampling/subreddit_codeset.txt", codeset)
    if os.path.exists("C:/Users/brend/Documents/diss/methods/sampling/subreddit_postset.txt") is False:
        print("building post set...")
        post_set = build_post_set()
        save_json("C:/Users/brend/Documents/diss/methods/sampling/subreddit_postset.txt", post_set)
    if os.path.exists("C:/Users/brend/Documents/diss/qualitative coding/subreddit_sample_top_posts.txt") is False:
        print("building sample set...")
        sample_post_set = sample_posts("C:/Users/brend/Documents/diss/methods/sampling/subreddit_postset.txt")
        save_json("C:/Users/brend/Documents/diss/qualitative coding/subreddit_sample_top_posts.txt", sample_post_set)
    if os.path.exists("C:/Users/brend/Documents/diss/qualitative coding/subreddit_sample_top_threads.txt") is False:
        print("extracting comments...")
        sample_set = extract_comments("C:/Users/brend/Documents/diss/qualitative coding/subreddit_sample_top_posts.txt")
        save("C:/Users/brend/Documents/diss/qualitative coding/subreddit_sample_top_threads.txt", sample_set)

main()

print("total runtime:", time_elapsed())