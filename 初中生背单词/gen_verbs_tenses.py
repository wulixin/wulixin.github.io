#!/usr/bin/env python3
"""初中生背单词 - 动词与时态题库生成器
生成 verbs.json (动词用法题库) 和 tenses.json (时态题库)
"""

import json
import random
import os

random.seed(42)

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(BASE_DIR, "data")

# ============================================================
# 工具函数
# ============================================================

def make_mc(id, cat, diff, q, hint, options, answer_idx, explain, examples, mnemonic):
    return {
        "id": id, "cat": cat, "diff": diff, "type": "mc",
        "q": q, "hint": hint, "options": options,
        "answer": answer_idx, "explain": explain,
        "examples": examples, "mnemonic": mnemonic
    }

def make_fill(id, cat, diff, q, hint, answer_text, explain, examples, mnemonic):
    return {
        "id": id, "cat": cat, "diff": diff, "type": "fill",
        "q": q, "hint": hint,
        "answer": answer_text, "explain": explain,
        "examples": examples, "mnemonic": mnemonic
    }

def shuffle_options(options, correct_idx):
    """打乱选项并返回新索引"""
    paired = list(enumerate(options))
    random.shuffle(paired)
    new_options = [p[1] for p in paired]
    new_correct = next(i for i, p in enumerate(paired) if p[0] == correct_idx)
    return new_options, new_correct

next_id = [1]
def nid():
    i = next_id[0]
    next_id[0] += 1
    return i


# ============================================================
# 第一部分: verbs.json - 动词用法题库 (目标 500 题)
# ============================================================

def gen_irregular_verbs():
    """不规则动词过去式/过去分词"""
    questions = []

    verbs_data = [
        # (原形, 过去式, 过去分词, 中文)
        ("be", "was/were", "been", "是"), ("become", "became", "become", "变成"),
        ("begin", "began", "begun", "开始"), ("blow", "blew", "blown", "吹"),
        ("break", "broke", "broken", "打破"), ("bring", "brought", "brought", "带来"),
        ("build", "built", "built", "建造"), ("buy", "bought", "bought", "买"),
        ("catch", "caught", "caught", "抓住"), ("choose", "chose", "chosen", "选择"),
        ("come", "came", "come", "来"), ("cost", "cost", "cost", "花费"),
        ("cut", "cut", "cut", "切"), ("do", "did", "done", "做"),
        ("draw", "drew", "drawn", "画"), ("drink", "drank", "drunk", "喝"),
        ("drive", "drove", "driven", "驾驶"), ("eat", "ate", "eaten", "吃"),
        ("fall", "fell", "fallen", "落下"), ("feel", "felt", "felt", "感觉"),
        ("fight", "fought", "fought", "战斗"), ("find", "found", "found", "发现"),
        ("fly", "flew", "flown", "飞"), ("forget", "forgot", "forgotten/forgot", "忘记"),
        ("get", "got", "gotten/got", "得到"), ("give", "gave", "given", "给"),
        ("go", "went", "gone", "去"), ("grow", "grew", "grown", "生长"),
        ("hang", "hung/hanged", "hung/hanged", "挂"), ("have/has", "had", "had", "有"),
        ("hear", "heard", "heard", "听见"), ("hide", "hid", "hidden/hid", "藏"),
        ("hit", "hit", "hit", "击中"), ("hold", "held", "held", "握住"),
        ("hurt", "hurt", "hurt", "伤害"), ("keep", "kept", "kept", "保持"),
        ("know", "knew", "known", "知道"), ("lay", "laid", "laid", "放置"),
        ("lead", "led", "led", "领导"), ("learn", "learnt/learned", "learnt/learned", "学习"),
        ("leave", "left", "left", "离开"), ("lend", "lent", "lent", "借出"),
        ("let", "let", "let", "让"), ("lie", "lay", "lain", "躺"),
        ("lose", "lost", "lost", "丢失"), ("make", "made", "made", "制作"),
        ("mean", "meant", "meant", "意味着"), ("meet", "met", "met", "遇见"),
        ("pay", "paid", "paid", "支付"), ("put", "put", "put", "放"),
        ("read", "read", "read", "读"), ("ride", "rode", "ridden", "骑"),
        ("ring", "rang", "rung", "响"), ("rise", "rose", "risen", "升起"),
        ("run", "ran", "run", "跑"), ("say", "said", "said", "说"),
        ("see", "saw", "seen", "看见"), ("sell", "sold", "sold", "卖"),
        ("send", "sent", "sent", "发送"), ("set", "set", "set", "设置"),
        ("shake", "shook", "shaken", "摇"), ("shine", "shone/shined", "shone/shined", "照耀"),
        ("show", "showed", "shown/showed", "展示"), ("shut", "shut", "shut", "关闭"),
        ("sing", "sang", "sung", "唱"), ("sit", "sat", "sat", "坐"),
        ("sleep", "slept", "slept", "睡"), ("smell", "smelt/smelled", "smelt/smelled", "闻"),
        ("speak", "spoke", "spoken", "说"), ("stand", "stood", "stood", "站"),
        ("steal", "stole", "stolen", "偷"), ("sweep", "swept", "swept", "扫"),
        ("swim", "swam", "swum", "游泳"), ("take", "took", "taken", "拿"),
        ("teach", "taught", "taught", "教"), ("tell", "told", "told", "告诉"),
        ("think", "thought", "thought", "想"), ("throw", "threw", "thrown", "扔"),
        ("understand", "understood", "understood", "理解"), ("wake", "woke/waked", "woken/waked", "醒"),
        ("wear", "wore", "worn", "穿"), ("win", "won", "won", "赢"),
        ("write", "wrote", "written", "写"),
    ]

    for v_base, v_past, v_pp, v_cn in verbs_data:
        # MC题：选过去式
        pp_cn = v_pp.split("/")[0] if "/" in v_pp else v_pp
        past_clean = v_past.split("/")[0] if "/" in v_past else v_past

        # 生成干扰项（从其他动词的过去式中随机选取）
        others = [vd[1].split("/")[0] for vd in verbs_data if vd[0] != v_base]

        # 题型1：填过去式
        d = "easy" if v_base in ["be","do","go","have","say","see","take","make","get","give"] else \
            "medium" if v_base in ["come","eat","know","think","write","bring","buy","tell"] else "hard"
        opts = random.sample(others, 3) + [past_clean]
        opts2, ai = shuffle_options(opts, 3)
        questions.append(make_mc(
            nid(), "verb", d,
            f"The past tense form of \"{v_base}\" is ___.",
            f"\"{v_base}\" 的过去式是？",
            opts2, ai,
            f"\"{v_base}\" 是不规则动词，其过去式为 \"{past_clean}\"。",
            [f"He <strong>{past_clean}</strong> to school yesterday.（他昨天去了学校。）"],
            f"{v_base} → {past_clean}"
        ))

        # 题型2：填过去分词
        pp_others = [vd[2].split("/")[0] for vd in verbs_data if vd[0] != v_base and "/" not in vd[2]]
        if len(pp_others) >= 3:
            opts3 = random.sample(pp_others, 3) + [pp_cn]
            opts4, ai2 = shuffle_options(opts3, 3)
            questions.append(make_mc(
                nid(), "verb", d,
                f"The past participle of \"{v_base}\" is ___.",
                f"\"{v_base}\" 的过去分词是？",
                opts4, ai2,
                f"\"{v_base}\" 是不规则动词，其过去分词为 \"{pp_cn}\"。",
                [f"She has <strong>{pp_cn}</strong> her homework.（她已经完成了作业。）"],
                f"{v_base} → {past_clean} → {pp_cn}"
            ))

        # 题型3：fill 题型 — 句子中填空（部分高频动词）
        if random.random() < 0.4:
            questions.append(make_fill(
                nid(), "verb", d,
                f"I ___ (go) to the park last Sunday.",
                "last Sunday 提示用过去式",
                past_clean if v_base == "go" else v_base,
                f"根据时间状语 last Sunday，应用 {v_base} 的过去式形式。",
                [f"I <strong>{'went' if v_base=='go' else past_clean}</strong> to the park last Sunday."],
                f"{v_base} → {past_clean}" if v_base=="go" else ""
            ))
            # 修正：只针对当前动词
            questions.pop()

    # 补充 fill 题型：句子填空
    sentences_for_fill = [
        ("go", "They ___ to Beijing last year.", "went", "last year 用一般过去时", "easy"),
        ("take", "She ___ a taxi to the airport.", "took", "take 的过去式是不规则变化 took", "easy"),
        ("see", "I ___ him at the party.", "saw", "see 的过去式是 saw", "easy"),
        ("eat", "He ___ all the cake.", "ate", "eat 的过去式是 ate", "easy"),
        ("come", "My friend ___ yesterday.", "came", "come 的过去式是 came", "medium"),
        ("give", "Mom ___ me a gift.", "gave", "give 的过去式是 gave", "medium"),
        ("write", "She ___ a letter.", "wrote", "write 的过去式是 wrote", "medium"),
        ("know", "I ___ the answer.", "knew", "know 的过去式是 knew", "hard"),
        ("think", "He ___ about it.", "thought", "think 的过去式是 thought", "hard"),
        ("buy", "They ___ a new car.", "bought", "buy 的过去式是 bought", "easy"),
        ("bring", "He ___ his book.", "brought", "bring 的过去式是 brought", "medium"),
        ("teach", "The teacher ___ us English.", "taught", "teach 的过去式是 taught", "medium"),
        ("catch", "The cat ___ a mouse.", "caught", "catch 的过去式是 caught", "hard"),
        ("fall", "The boy ___ down.", "fell", "fall 的过去式是 fell", "easy"),
        ("feel", "I ___ tired.", "felt", "feel 的过去式是 felt", "easy"),
        ("leave", "She ___ early.", "left", "leave 的过去式是 left", "easy"),
        ("lose", "I ___ my key.", "lost", "lose 的过去式是 lost", "medium"),
        ("meet", "We ___ at the cafe.", "met", "meet 的过去式是 met", "easy"),
        ("send", "He ___ an email.", "sent", "send 的过去式是 sent", "medium"),
        ("win", "Our team ___ the game.", "won", "win 的过去式是 won", "easy"),
        ("sell", "She ___ her old bike.", "sold", "sell 的过去式是 sold", "medium"),
        ("tell", "He ___ me a story.", "told", "tell 的过去式是 told", "easy"),
        ("sit", "He ___ on the chair.", "sat", "sit 的过去式是 sat", "easy"),
        ("stand", "She ___ up.", "stood", "stand 的过去式是 stood", "easy"),
        ("understand", "I ___ you.", "understood", "understand 的过去式是 understood", "hard"),
        ("spend", "He ___ much money.", "spent", "spend 的过去式是 spent", "medium"),
        ("cut", "She ___ the paper.", "cut", "cut 的过去式还是 cut（不变）", "hard"),
        ("put", "He ___ it on the table.", "put", "put 的过去式还是 put（不变）", "hard"),
        ("read", "She ___ the book.", "read", "read 的过去式拼写不变，读音不同 /red/", "hard"),
        ("run", "The boy ___ fast.", "ran", "run 的过去式是 ran", "medium"),
        ("swim", "She ___ in the pool.", "swam", "swim 的过去式是 swam", "medium"),
        ("sing", "He ___ a song.", "sang", "sing 的过去式是 sang", "medium"),
        ("throw", "He ___ the ball.", "threw", "throw 的过去式是 threw", "hard"),
        ("fly", "The bird ___ away.", "flew", "fly 的过去式是 flew", "hard"),
        ("grow", "The plant ___ tall.", "grew", "grow 的过去式是 grew", "medium"),
        ("drive", "Dad ___ the car.", "drove", "drive 的过去式是 drove", "hard"),
        ("ring", "The bell ___.", "rang", "ring 的过去式是 rang", "medium"),
        ("wake", "I ___ up early.", "woke", "wake 的过去式是 woke", "medium"),
        ("steal", "Someone ___ my wallet.", "stole", "steal 的过去式是 stole", "hard"),
        ("choose", "She ___ the blue dress.", "chose", "choose 的过去式是 chose", "hard"),
        ("speak", "He ___ English well.", "spoke", "speak 的过去式是 spoke", "medium"),
        ("break", "He ___ the window.", "broke", "break 的过去式是 broke", "medium"),
        ("forget", "I ___ my homework.", "forgot", "forget 的过去式是 forgot", "hard"),
        ("hold", "She ___ my hand.", "held", "hold 的过去式是 held", "medium"),
        ("shut", "He ___ the door.", "shut", "shut 的过去式还是 shut（不变）", "hard"),
        ("sleep", "The baby ___ well.", "slept", "sleep 的过去式是 slept", "easy"),
        ("sweep", "She ___ the floor.", "swept", "sweep 的过去式是 swept", "hard"),
        ("pay", "He ___ for lunch.", "paid", "pay 的过去式是 paid", "medium"),
        ("build", "They ___ a house.", "built", "build 的过去式是 built", "medium"),
        ("lend", "She ___ me some money.", "lent", "lend 的过去式是 lent", "hard"),
        ("mean", "What do you ___?", "meant", "mean 的过去式是 meant", "hard"),
        ("hide", "The cat ___ under the bed.", "hid", "hide 的过去式是 hid", "hard"),
        ("fight", "They ___ bravely.", "fought", "fight 的过去式是 fought", "hard"),
        ("hurt", "He ___ his leg.", "hurt", "hurt 的过去式还是 hurt（不变）", "hard"),
        ("cost", "It ___ ten dollars.", "cost", "cost 的过去式还是 cost（不变）", "hard"),
        ("hit", "The ball ___ me.", "hit", "hit 的过去式还是 hit（不变）", "hard"),
    ]
    for verb, sent, ans, hint_text, diff in sentences_for_fill:
        questions.append(make_fill(
            nid(), "verb", diff,
            sent, hint_text, ans,
            f"此句需用 {verb} 的过去式 \"{ans}\"。{verb} 是不规则动词，必须牢记其变形。",
            [sent.replace("___", f"<strong>{ans}</strong>")],
            f"{verb} → {ans}"
        ))

    return questions


def gen_transitive_intransitive():
    """及物/不及物动词用法"""
    qs = []
    data = [
        # (动词, 及物性, 用法说明, 正确选项, 干扰项, 解析, 例句, 口诀)
        ("arrive", "intransitive", "arrive 后接地点需加介词", "arrive at/in",
         ["arrive to", "arrive with", "arrive on"],
         "arrive 是不及物动词，后接小地点用 at，大地点用 in。",
         ["We arrived <strong>at</strong> the station.（我们到了车站。）",
          "They arrived <strong>in</strong> London.（他们到达了伦敦。）"],
         "arrive 不及物，at 小 in 大"),
        ("listen", "intransitive", "listen 后接宾语需加 to", "listen to",
         ["listen", "listen with", "listen at"],
         "listen 是不及物动词，表示\"听...\"时必须加 to。",
         ["Please <strong>listen to</strong> me.（请听我说。）",
          "He likes listening <strong>to</strong> music.（他喜欢听音乐。）"],
         "listen 不及物，to 来帮"),
        ("wait", "intransitive", "wait 后接宾语需加 for", "wait for",
         ["wait", "wait to", "wait on"],
         "wait 作不及物动词，表示\"等待某人/某事\"时需加 for。",
         ["I'm <strong>waiting for</strong> you.（我在等你。）",
          "Please wait <strong>for</strong> me.（请等我一下。）"],
         "wait 不及物，for 相伴"),
        ("laugh", "both", "laugh at 表示嘲笑", "laugh at",
         ["laugh to", "laugh with", "laugh on"],
         "\"laugh at sb.\" 表示\"嘲笑某人\"；laugh 单独使用表示\"大笑\"。",
         ["Don't <strong>laugh at</strong> others.（不要嘲笑别人。）",
          "They laughed <strong>at</strong> his joke.（他们因他的笑话而大笑。）"],
         "嘲笑某人 laugh at"),
        ("look", "both", "look at 表示看...", "look at",
         ["look", "look to", "look with"],
         "look 作不及物动词，表示\"看...\"时需加 at；作系动词表示\"看起来\"。",
         ["<strong>Look at</strong> the blackboard!（看黑板！）",
          "She <strong>looks at</strong> the picture.（她正在看那张画。）"],
         "看什么 look at"),
        ("smile", "intransitive", "smile at 对着...笑", "smile at",
         ["smile to", "smile with", "smile on"],
         "smile 是不及物动词，表示\"对某人微笑\"时需加 at。",
         ["She smiled <strong>at</strong> me.（她对我微笑了。）",
          "He smiles <strong>at</strong> everyone.（他对每个人微笑。）"],
         "微笑 smile at"),
        ("ask", "transitive", "ask 直接跟双宾语", "ask sb. sth.",
         ["ask to sb. sth.", "ask for sb. sth.", "ask with sb."],
         "ask 是及物动词，可接双宾语 ask sb. sth.（问某人某事）。",
         ["Can I <strong>ask</strong> you a question?（我可以问你一个问题吗？）",
          "He asked <strong>me</strong> the way.（他问我路怎么走。）"],
         "问人问事 ask 双宾"),
        ("answer", "transitive", "answer 直接跟宾语", "answer the question",
         ["answer to the question", "answer for the question", "answer with the question"],
         "answer 是及物动词，直接接宾语，不需要加 to。",
         ["Please <strong>answer</strong> my question.（请回答我的问题。）",
          "He answered <strong>the door</strong>.（他去开门了。）"],
         "回答直接 answer，不加 to"),
        ("enter", "transitive", "enter 直接接地点", "enter the room",
         ["enter into the room", "enter to the room", "enter in the room"],
         "enter 是及物动词，直接接地点名词，不需要加 into（into 多余但不算错）。",
         ["Please <strong>enter</strong> the room.（请进入房间。）",
          "They entered <strong>the building</strong>.（他们进入了大楼。）"],
         "进入 enter 直接地"),
        ("serve", "transitive", "serve 直接接宾语", "serve the people",
         ["serve for the people", "serve to the people", "serve with the people"],
         "serve 是及物动词，直接接服务对象，不需要加 for。",
         ["We should <strong>serve</strong> the people.（我们应该为人民服务。）",
          "She serves <strong>dinner</strong>.（她端上晚餐。）"],
         "服务 serve 直接接"),
        ("marry", "transitive", "marry 直接接宾语", "marry sb.",
         ["marry with sb.", "marry to sb.", "marry for sb."],
         "marry 是及物动词，直接接结婚对象，不需要加 with/to。",
         ["He married <strong>a teacher</strong>.（他和一位老师结婚了。）",
          "She wants to marry <strong>him</strong>.（她想嫁给他。）"],
         "嫁娶 marry 直接连"),
        ("discuss", "transitive", "discuss 直接接宾语", "discuss the problem",
         ["discuss about the problem", "discuss on the problem", "discuss over the problem"],
         "discuss 是及物动词，直接接讨论的话题，不需要加 about。",
         ["Let's <strong>discuss</strong> this problem.（我们来讨论这个问题。）",
          "They discussed <strong>the plan</strong>.（他们讨论了计划。）"],
         "讨论 discuss 无 about"),
        ("reach", "transitive", "reach 直接接地点", "reach the school",
         ["reach to the school", "reach at the school", "reach in the school"],
         "reach 是及物动词，直接接到达的地点，不需要加任何介词。",
         ["We reached <strong>the station</strong> on time.（我们按时到达了车站。）",
          "Can you reach <strong>the shelf</strong>?（你能够得着那个架子吗？）"],
         "到达 reach 直接带"),
        ("call", "both", "call sb. 给某人打电话", "call me",
         ["call to me", "call for me", "call with me"],
         "call 作及物动词时可直接接宾语表示\"给某人打电话\"。",
         ["Please <strong>call</strong> me tonight.（今晚给我打电话。）",
          "I'll call <strong>you</strong> tomorrow.（我明天给你打电话。）"],
         "打电话 call 直接加"),
        ("follow", "transitive", "follow 直接接宾语", "follow him",
         ["follow to him", "follow after him", "follow with him"],
         "follow 是及物动词，直接接跟随的对象。",
         ["Please <strong>follow</strong> me.（请跟我来。）",
          "Follow <strong>your heart</strong>.（追随你的内心。）"],
         "跟随 follow 直接跟"),
        ("mention", "transitive", "mention 直接接宾语", "mention it",
         ["mention about it", "mention of it", "mention to it"],
         "mention 是及物动词，直接接提到的事物，不需要加 about。",
         ["Don't <strong>mention</strong> it!（不客气！）",
          "He mentioned <strong>the meeting</strong>.（他提到了会议。）"],
         "提及 mention 无介词"),
        ("contact", "transitive", "contact 直接接宾语", "contact him",
         ["contact with him", "contact to him", "contact for him"],
         "contact 是及物动词，直接接联系对象。",
         ["Please <strong>contact</strong> me soon.（请联系我。）",
          "You can contact <strong>the office</strong>.（你可以联系办公室。）"],
         "联系 contact 直接连"),
    ]

    for verb, trans_type, usage, correct, distractors, explain, exs, mn in data:
        d = "easy"
        opts = distractors + [correct]
        o, ai = shuffle_options(opts, len(distractors))
        qs.append(make_mc(nid(), "verb", d,
            f"Which sentence is correct?",
            f"{verb} 的正确用法",
            o, ai, explain, exs, mn))

        # 每个配一个 fill 题
        parts = correct.split()
        blank_verb = parts[0]
        qs.append(make_fill(nid(), "verb", d,
            f"Please ___ me when you arrive. ({blank_verb})",
            f"{blank_verb} 的用法",
            correct.split()[-1] if len(parts) > 1 else blank_verb,
            explain, exs, mn))

    return qs


def gen_verb_pairs():
    """高频动词辨析"""
    qs = []

    # ===== say vs tell vs speak vs talk =====
    say_tell_data = [
        ("She always ___ a story before bedtime.", "tells", ["says", "speaks", "talks"],
         "tell a story 是固定搭配，表示\"讲故事\"。",
         ["Mom always <strong>tells</strong> us a story.（妈妈总是给我们讲故事。）"],
         "讲故事 tell a story"),
        ("Can you ___ it in English?", "say", ["tell", "speak", "talk"],
         "say 强调说话的内容，后接说的具体话语。",
         ["Can you <strong>say</strong> it in English?（你能用英语说吗？）",
          "He said \"hello\".（他说了\"你好\"。）"],
         "说什么 say 内容"),
        ("He can ___ three languages.", "speak", ["say", "tell", "talk"],
         "speak + 语言，表示\"会说某种语言\"。",
         ["She can <strong>speak</strong> Chinese.（她会说中文。）",
          "Do you <strong>speak</strong> English?（你会说英语吗？）"],
         "说语言 speak + 语言"),
        ("Don't ___ loudly in class!", "talk", ["say", "tell", "speak"],
         "talk 表示\"交谈、聊天\"，强调双方互动。",
         ["Stop <strong>talking</strong>, please!（请别说话了！）",
          "They are talking about the movie.（他们在谈论那部电影。）"],
         "闲聊交谈用 talk"),
        ("The teacher ___ us to be quiet.", "told", ["said", "spoke", "talked"],
         "tell sb. to do sth. 是固定结构，表示\"告诉某人做某事\"。",
         ["He told <strong>me</strong> to wait.（他叫我等一下。）"],
         "告诉某人做 tell sb. to do"),
        ("___ yourself!", "Say", ["Tell", "Speak", "Talk"],
         "\"Say to yourself\" 或 \"Say it\" 用于自言自语或表达内容。",
         ["<strong>Say</strong> it again!（再说一遍！）",
          "What did he <strong>say</strong>?（他说了什么？）"],
         "表达内容用 say"),
    ]

    for sent, ans, dist, exp, exs, mn in say_tell_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "medium", sent, "", o, ai, exp, exs, mn))

    # ===== see vs look vs watch =====
    look_see_data = [
        ("I ___ but saw nothing.", "looked", ["watched", "saw", "noticed"],
         "look 表示\"看的动作\"（不及物），强调主动去看。",
         ["I <strong>looked</strong> everywhere.（我到处看了。）"],
         "动作 look，结果 see"),
        ("Did you ___ that movie last night?", "watch", ["see", "look", "notice"],
         "watch 表示\"观看\"移动的画面、比赛、表演等。",
         ["Let's <strong>watch</strong> a movie.（我们看电影吧。）",
          "He is watching TV.（他在看电视。）"],
         "看电视比赛用 watch"),
        ("I ___ him crossing the street.", "saw", ["looked", "watched", "noticed"],
         "see 强调\"看到的结果\"，表示眼睛看到了。",
         ["I <strong>saw</strong> a bird.（我看到了一只鸟。）",
          "Can you <strong>see</strong> it?（你能看到它吗？）"],
         "看到结果用 see"),
        ("___ at the blackboard, please.", "Look", ["See", "Watch", "Notice"],
         "look 常用于祈使句引起注意。",
         ["<strong>Look</strong> here!（看这里！）",
          "<strong>Look</strong> at me!（看着我！）"],
         "引人注意 Look at"),
        ("She likes ___ the sunset.", "watching", ["seeing", "looking", "noticing"],
         "watch 可以用于观看静态但有变化的场景（如日落）。",
         ["I like watching <strong>sunsets</strong>.（我喜欢看日落。）"],
         "观赏场景用 watch"),
    ]

    for sent, ans, dist, exp, exs, mn in look_see_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "medium", sent, "", o, ai, exp, exs, mn))

    # ===== hear vs listen =====
    hear_listen_data = [
        ("I can ___ someone crying.", "hear", ["listen", "listen to", "sound"],
         "hear 强调\"听到\"的结果，不一定是有意去听。",
         ["I can <strong>hear</strong> music.（我能听到音乐声。）",
          "Did you <strong>hear</strong> that?（你听到了吗？）"],
         "无意听到 hear"),
        ("He ___ to music every evening.", "listens", ["hears", "hears to", "sounds"],
         "listen to 表示\"有意倾听\"，强调听的主动性和持续性。",
         ["I like listening <strong>to</strong> music.（我喜欢听音乐。）",
          "Listen <strong>to</strong> me carefully.（仔细听我说。）"],
         "有意倾听 listen to"),
        ("I ___ a noise outside.", "heard", ["listened", "listened to", "sounded"],
         "hear 表示耳朵接收到的声音（结果），不需要 to。",
         ["I heard <strong>a strange noise</strong>.（我听到一声怪响。）"],
         "听到声音 heard"),
    ]

    for sent, ans, dist, exp, exs, mn in hear_listen_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "easy", sent, "", o, ai, exp, exs, mn))

    # ===== bring vs take =====
    bring_take_data = [
        ("Please ___ your book tomorrow.", "bring", ["take", "carry", "fetch"],
         "bring 表示\"带来\"（朝向说话者方向）；take 表示\"带走\"（远离说话者）。",
         ["Remember to <strong>bring</strong> your homework.（记得把作业带来。）",
          "Bring <strong>it</strong> here.（把它带到这儿来。）"],
         "拿来 bring，带走 take"),
        ("Don't forget to ___ an umbrella.", "take", ["bring", "carry", "fetch"],
         "take 表示\"随身携带/带走\"（远离当前位置）。",
         ["Take <strong>an umbrella</strong> with you.（带把伞吧。）",
          "Take <strong>your coat</strong>.（带上你的外套。）"],
         "随身带走 take"),
        ("She ___ some flowers to the party.", "brought", ["took", "carried", "fetched"],
         "bring 的过去式 brought，表示\"带到（某个地方）\"。",
         ["She brought <strong>some cakes</strong>.（她带了一些蛋糕来。）"],
         "带来 brought (bring的过去式)"),
    ]

    for sent, ans, dist, exp, exs, mn in bring_take_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "medium", sent, "", o, ai, exp, exs, mn))

    # ===== borrow vs lend =====
    borrow_lend_data = [
        ("Can I ___ your pen?", "borrow", ["lend", "keep", "rent"],
         "borrow 表示\"借入\"（向别人借），常与 from 连用。",
         ["Can I <strong>borrow</strong> your bike?（我可以借你的自行车吗？）",
          "I borrowed a book <strong>from</strong> the library.（我从图书馆借了一本书。）"],
         "借入 borrow (from)"),
        ("Could you ___ me five dollars?", "lend", ["borrow", "rent", "keep"],
         "lend 表示\"借出\"（借给别人），可接双宾语 lend sb. sth.。",
         ["Can you <strong>lend</strong> me your ruler?（能借我把尺子吗？）",
          "She lent <strong>me</strong> her notes.（她借给了我她的笔记。）"],
         "借出 lend (sb.)"),
        ("May I ___ your dictionary?", "borrow", ["lend", "keep", "use"],
         "主语是 I，表示\"我想借入\"，用 borrow。",
         ["I want to <strong>borrow</strong> a book.（我想借一本书。）"],
         "主语想借入用 borrow"),
    ]

    for sent, ans, dist, exp, exs, mn in borrow_lend_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "easy", sent, "", o, ai, exp, exs, mn))

    # ===== rise vs raise =====
    rise_raise_data = [
        ("The sun ___ in the east.", "rises", ["raises", "raised", "rising"],
         "rise 是不及物动词（无被动），表示\"上升、升起\"。",
         ["The sun <strong>rises</strong> in the east.（太阳从东方升起。）",
          "Prices are rising.（价格在上涨。）"],
         "自然升起 rise (不及物)"),
        ("Please ___ your hand if you know the answer.", "raise", ["rise", "lift", "rise up"],
         "raise 是及物动词，表示\"举起、抬高\"，后面必须接宾语。",
         ["Please raise <strong>your hand</strong>.（请举手。）",
          "He raised his voice.（他提高了声音。）"],
         "举手抬高 raise (及物)"),
        ("Her voice ___ when she got excited.", "rose", ["raised", "raises", "has raised"],
         "voice 不能被\"举起\"，它是自己\"升高\"，所以用不及物的 rose。",
         ["His voice <strong>rose</strong> in anger.（他的声音因愤怒而提高。）"],
         "声音升高用 rose"),
    ]

    for sent, ans, dist, exp, exs, mn in rise_raise_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "hard", sent, "", o, ai, exp, exs, mn))

    # ===== lie (躺) vs lay (放) =====
    lie_lay_data = [
        ("He ___ on the sofa and fell asleep.", "lay", ["lied", "laid", "has lain"],
         "lie（躺）的过去式是 lay（注意不是 lied，lied 是\"撒谎\"的过去式）。",
         ["He <strong>lay</strong> in bed all day.（他躺了一整天。）",
          "Lay down, please.（请躺下。）"],
         "躺的过去式是 lay"),
        ("The hen ___ an egg every day.", "lays", ["lies", "laid", "lying"],
         "lay（下蛋/放置）的第三人称单数是 lays。",
         ["The hen <strong>lays</strong> eggs.（母鸡下蛋。）"],
         "下蛋 lays (lay的单三)"),
        ("She ___ the book on the desk.", "laid", ["lay", "lied", "has lain"],
         "lay（放置）的过去式是 laid，表示\"把...放在...上\"。",
         ["She <strong>laid</strong> the baby down.（她把婴儿放下了。）"],
         "放的过去式 laid"),
        ("Don't ___ to me!", "lie", ["lay", "laid", "lain"],
         "lie（撒谎）是规则动词，此处是原形命令句。",
         ["Never <strong>lie</strong> to your parents.（永远不要对父母撒谎。）",
          "He lied to me.（他对我撒谎了。）"],
         "撒谎 lie (规则变化)"),
    ]

    for sent, ans, dist, exp, exs, mn in lie_lay_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "hard", sent, "", o, ai, exp, exs, mn))

    # ===== spend / cost / take / pay 花费 =====
    spend_cost_data = [
        ("I ___ two hours doing my homework.", "spent", ["cost", "took", "paid"],
         "sb. spends time/money (on) doing sth. 人作主语。",
         ["I spent <strong>an hour</strong> reading.（我花了一小时阅读。）"],
         "人花费 spent"),
        ("The shirt ___ me 50 yuan.", "cost", ["spent", "took", "paid"],
         "sth. costs sb. money 物作主语，且 cost 只能用物作主语。",
         ["The bike <strong>cost</strong> me 300 yuan.（这辆自行车花了我300元。）"],
         "物花费 cost"),
        ("It ___ us three days to finish the work.", "took", ["spent", "cost", "paid"],
         "It takes sb. time to do sth. 形式主语 It。",
         ["It takes <strong>time</strong> to learn.（学习是需要时间的。）"],
         "做事花费 It takes"),
        ("She ___ 100 yuan for the dress.", "paid", ["spent", "cost", "took"],
         "sb. pays money for sth. 人作主语，pay 后接钱再接 for。",
         ["I paid <strong>20 yuan</strong> for the ticket.（我花了20元买票。）"],
         "付钱 pay ... for"),
    ]

    for sent, ans, dist, exp, exs, mn in spend_cost_data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "hard", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_make_vs_do():
    """make vs do 搭配"""
    qs = []
    data = [
        ("Let's ___ a cake for Mom's birthday.", "make", ["do", "take", "have"],
         "make 表示\"制作、制造\"，make a cake 烤蛋糕。",
         ["She made <strong>a delicious dinner</strong>.（她做了一顿美味的晚餐。）"],
         "制造 make"),
        ("I must ___ my homework first.", "do", ["make", "take", "get"],
         "do one's homework 做作业（固定搭配）。",
         ["Have you done <strong>your homework</strong>?（你做完作业了吗？）"],
         "作业 do homework"),
        ("Can you ___ a favor for me?", "do", ["make", "take", "give"],
         "do sb. a favor 帮某人一个忙（固定搭配）。",
         ["Can you do <strong>me a favor</strong>?（能帮我个忙吗？）"],
         "帮忙 do a favor"),
        ("Don't ___ so much noise!", "make", ["do", "take", "have"],
         "make noise 发出噪音（固定搭配）。",
         ["Don't make <strong>noise</strong> in class.（不要在课上发出噪音。）"],
         "噪音 make noise"),
        ("He ___ a mistake in the exam.", "made", ["did", "took", "had"],
         "make a mistake犯错（固定搭配）。",
         ["I made <strong>a silly mistake</strong>.（我犯了一个愚蠢的错误。）"],
         "犯错 make mistake"),
        ("She ___ her best in the competition.", "did", ["made", "took", "had"],
         "do one's best 尽最大努力（固定搭配）。",
         ["Just do <strong>your best</strong>.（尽你最大的努力就好。）"],
         "尽力 do one's best"),
        ("Let's ___ a decision together.", "make", ["do", "take", "have"],
         "make a decision 做决定（固定搭配）。",
         ["We need to make <strong>a decision</strong>.（我们需要做一个决定。）"],
         "决定 make decision"),
        ("What do you usually ___ on weekends?", "do", ["make", "take", "have"],
         "询问活动通常用 What do you do?",
         ["What do you <strong>do</strong> after school?（你放学后做什么？）"],
         "做什么活动用 do"),
        ("The news ___ her very happy.", "made", ["did", "took", "got"],
         "make + 宾语 + 形容词/名词，表示\"使...怎么样\"。",
         ["The movie made <strong>me cry</strong>.（这部电影让我哭了。）"],
         "使... make + obj + adj/n"),
        ("He ___ exercises every morning.", "does", ["makes", "takes", "gets"],
         "do exercise(s) 做运动/锻炼（固定搭配）。",
         ["You should do <strong>more exercise</strong>.（你应该多做运动。）"],
         "锻炼 do exercise"),
        ("Can you ___ the bed?", "make", ["do", "take", "clean"],
         "make the bed 整理床铺（固定搭配）。",
         ["Make your <strong>bed</strong> after you get up.（起床后整理床铺。）"],
         "整理床铺 make bed"),
        ("I need to ___ some shopping.", "do", ["make", "take", "get"],
         "do some shopping 购物（固定搭配）。",
         ["Mom went to do <strong>some shopping</strong>.（妈妈去买东西了。）"],
         "购物 do shopping"),
        ("She ___ progress in English.", "made", ["did", "took", "got"],
         "make progress 取得进步（固定搭配，progress 不可数，不用 a）。",
         ["You've made great <strong>progress</strong>.（你取得了很大进步。）"],
         "取得进步 make progress"),
        ("Please ___ room for me.", "make", ["do", "take", "give"],
         "make room for 给...腾地方（固定搭配）。",
         ["Can you make <strong>room</strong> for this box?（能给这个盒子腾点空间吗？）"],
         "腾地 make room for"),
        ("He ___ friends easily.", "makes", ["does", "takes", "gets"],
         "make friends 交朋友（固定搭配）。",
         ["It's easy to make <strong>friends</strong> here.（在这里很容易交朋友。）"],
         "交朋友 make friends"),
        ("I ___ a phone call to him.", "made", ["did", "took", "gave"],
         "make a phone call 打电话（固定搭配）。",
         ["Let me make <strong>a call</strong>.（让我打个电话。）"],
         "打电话 make a call"),
        ("She ___ a face at me.", "made", ["did", "took", "gave"],
         "make a face 做鬼脸（固定搭配）。",
         ["The baby made <strong>a funny face</strong>.（宝宝做了个滑稽的鬼脸。）"],
         "做鬼脸 make a face"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        d = "easy" if ans in ("make","do") and any(w in sent for w in ["homework","cake","noise","bed"]) else "medium"
        qs.append(make_mc(nid(), "verb", d, sent, "", o, ai, exp, exs, mn))

    return qs


def gen_have_take_get_collocations():
    """have/take/get 常见搭配"""
    qs = []
    data = [
        ("I usually ___ breakfast at 7:00.", "have", ["take", "get", "make"],
         "have breakfast 吃早餐（三餐都用 have）。",
         ["I have <strong>lunch</strong> at school.（我在学校吃午饭。）"],
         "三餐 have breakfast/lunch/dinner"),
        ("Let's ___ a break.", "take", ["have", "get", "make"],
         "take a break 休息一下（固定搭配）。",
         ["Let's take <strong>a short break</strong>.（我们休息一下吧。）"],
         "休息 take a break"),
        ("She ___ a cold last week.", "had", ["took", "got", "caught"],
         "have a cold 感冒（固定搭配，也可用 catch a cold）。",
         ["I have <strong>a fever</strong>.（我发烧了。）"],
         "生病 have a cold/fever"),
        ("Can you ___ a photo of us?", "take", ["have", "get", "make"],
         "take a photo/picture 照相（固定搭配）。",
         ["Let's take <strong>a photo</strong> together!（我们一起拍张照吧！）"],
         "照相 take a photo"),
        ("I ___ an email from my pen pal.", "got", ["had", "took", "received"],
         "get 收到（口语常用，相当于 receive）。",
         ["I got <strong>a letter</strong> yesterday.（我昨天收到了一封信。）"],
         "收到 get (a letter/email)"),
        ("He ___ a shower every morning.", "takes", ["has", "gets", "makes"],
         "take a shower 淋浴（也可用 have a shower）。",
         ["I take a shower <strong>every morning</strong>.（我每天早上洗澡。）"],
         "淋浴 take/have a shower"),
        ("We should ___ pity on them.", "have", ["take", "get", "make"],
         "have pity on 怜悯（固定搭配）。",
         ["Have pity <strong>on</strong> him.（可怜可怜他吧。）"],
         "怜悯 have pity on"),
        ("She ___ part in the singing contest.", "took", ["had", "got", "made"],
         "take part in 参加（活动）（固定搭配）。",
         ["I want to take part <strong>in</strong> the race.（我想参加赛跑。）"],
         "参加 take part in"),
        ("I ___ a headache.", "have", ["take", "get", "catch"],
         "have a headache 头疼（身体不适用 have）。",
         ["I have <strong>a stomachache</strong>.（我胃疼。）"],
         "头疼/胃疼 have a ...ache"),
        ("The plane ___ off at 8:00.", "takes", ["has", "gets", "makes"],
         "take off 起飞（固定搭配）。",
         ["The plane will take <strong>off</strong> soon.（飞机很快就要起飞了。）"],
         "起飞 take off"),
        ("Let me ___ care of it.", "take", ["have", "get", "make"],
         "take care of 照顾、处理（固定搭配）。",
         ["I'll take care <strong>of</strong> the dog.（我会照顾这只狗的。）"],
         "照顾 take care of"),
        ("He ___ angry easily.", "gets", ["has", "takes", "makes"],
         "get + 形容词 表示变得...（系动词变化）。",
         ["It's getting <strong>dark</strong>.（天变黑了。）",
          "Don't get <strong>angry</strong>.（别生气。）"],
         "变得 get + adj"),
        ("I ___ a good idea!", "have", ["get", "take", "make"],
         "have an idea 有一个主意。",
         ["I have <strong>a better idea</strong>.（我有个更好的主意。）"],
         "有主意 have an idea"),
        ("She ___ dressed quickly.", "gets", ["has", "takes", "makes"],
         "get dressed 穿衣（固定搭配）。",
         ["Get dressed <strong>quickly</strong>!（快点穿好衣服！）"],
         "穿衣 get dressed"),
        ("It ___ me two hours to get there.", "took", ["had", "got", "spent"],
         "It takes sb. time to do sth. 做某事花了某人多少时间。",
         ["It took <strong>us</strong> an hour.（我们花了一小时。）"],
         "花费时间 It takes"),
        ("Don't ___ it seriously.", "take", ["have", "get", "make"],
         "take...seriously 认真对待...",
         ["You should take <strong>this seriously</strong>.（你应该认真对待这件事。）"],
         "认真对待 take seriously"),
        ("I ___ notice of the sign.", "took", ["had", "got", "made"],
         "take notice of 注意到（固定搭配）。",
         ["Did you take notice <strong>of</strong> the warning?（你注意到那个警告了吗？）"],
         "注意到 take notice of"),
        ("She ___ lost on her way home.", "got", ["had", "took", "became"],
         "get lost 迷路（固定搭配）。",
         ["I got <strong>lost</strong> in the city.（我在城里迷路了。）"],
         "迷路 get lost"),
        ("We should ___ pride in our work.", "take", ["have", "get", "make"],
         "take pride in 以...为自豪（固定搭配）。",
         ["Take pride <strong>in</strong> your work!（为你的工作感到自豪！）"],
         "以...为荣 take pride in"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "medium", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_modal_verbs():
    """情态动词"""
    qs = []
    data = [
        ("___ you help me, please?", "Can", ["Do", "Are", "Will"],
         "can 表示请求许可或能力，Can I/you...? 是常见礼貌用语。",
         ["<strong>Can</strong> you pass me the salt?（能把盐递给我吗？）"],
         "请求帮忙 Can you...?"),
        ("You ___ not smoke here.", "must", ["need", "can", "do"],
         "must not 表示禁止（语气最强）。",
        ["You mustn't <strong>park</strong> here.（禁止在此停车。）"],
         "禁止 must not"),
        ("___ I come in?", "May", ["Must", "Can", "Do"],
         "may I...? 比 Can I...? 更正式委婉，用于请求许可。",
         ["<strong>May</strong> I use your phone?（我可以用你的手机吗？）"],
         "正式请求 May I...?"),
        ("It ___ rain later today.", "might", ["must", "will", "can"],
         "might 表示可能性较小的推测，\"可能、也许\"。",
         ["He might <strong>be</strong> late.（他可能会迟到。）"],
         "不太确定 might"),
        ("You ___ see a doctor.", "should", ["can", "would", "could"],
         "should 表示建议，\"应该\"",
         ["You should <strong>study</strong> harder.（你应该更努力学习。）"],
         "建议应该 should"),
        ("___ you like some tea?", "Would", ["Will", "Could", "Should"],
         "Would you like...? 表示客气邀请/提议。",
         ["Would you like <strong>some coffee</strong>?（你想来点咖啡吗？）"],
         "客气邀请 Would you like...?"),
        ("She ___ swim when she was five.", "could", ["can", "may", "might"],
         "could 是 can 的过去式，表示过去的能力。",
         ["I could <strong>ride</strong> a bike at six.（我六岁就会骑自行车了。）"],
         "过去能力 could"),
        ("That ___ be true—it's impossible!", "can't", ["mustn't", "might not", "shouldn't"],
         "can't be 表示有把握的否定推测，\"不可能是\"。",
         ["That can't <strong>be</strong> right!（那不可能是对的！）"],
         "不可能 can't be"),
        ("You ___ worry about it.", "needn't", ["mustn't", "can't", "won't"],
         "needn't (= need not) 表示\"不必\"，与 must（必须）相对。",
         ["You needn't <strong>finish</strong> it today.（你今天不必完成它。）"],
         "不必 needn't"),
        ("He ___ be in the office—the light is on.", "must", ["can", "may", "could"],
         "must be 表示有把握的肯定推测，\"一定是、准是\"。",
         ["She must <strong>be</strong> tired.（她一定累了。）"],
         "一定肯定 must be"),
        ("___ we go now?", "Shall", ["Will", "Should", "Would"],
         "Shall we...? 表示提出建议（多用于第一人称疑问句）。",
         ["<strong>Shall</strong> we dance?（我们可以跳舞吗？）"],
         "提建议 Shall we...?"),
        ("I ___ rather stay home.", "'d", ["will", "shall", "should"],
         "would rather (= 'd rather) 表示\"宁愿\"。",
         ["I'd rather <strong>go</strong> by bus.（我宁愿坐公交车去。）"],
         "宁愿 would rather"),
        ("You ___ better hurry up.",("'d")+" better", ["have", "should", "can"],
         "had better 表示\"最好\"（劝告，语气较强）。",
         ["You'd better <strong>go</strong> now.（你最好现在就走。）"],
         "最好 had better"),
        ("Students ___ respect teachers.", "ought to", ["must", "should", "need"],
         "ought to = should，表示义务或建议。",
         ["You ought to <strong>apologize</strong>.（你应该道歉。）"],
         "应该 ought to = should"),
        ("How ___ you say that!", "dare", ["need", "must", "can"],
         "dare 敢（情态动词用法），How dare you! 你竟敢...!",
         ["How dare <strong>you</strong> speak to me like that!（你怎么敢那样跟我说话！）"],
         "敢 dare"),
        ("You ___ use my computer.", "can", ["must", "need", "will"],
         "can 表示许可，\"可以、允许\"。",
         ["You can <strong>use</strong> my pen.（你可以用我的笔。）"],
         "许可 can"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", "medium", sent, "", o, ai, exp, exs, mn))

    # 补充 fill 题目
    modal_fills = [
        ("___ (can/could) you tell me the way to the station?", "Could", "Could 比 Can 更礼貌",
         "请求帮忙 Could you... 更礼貌", "easy"),
        ("You ___ (must) finish it today.", "mustn't", "mustn't = 禁止，must = 必须",
         "mustn't 绝对不能", "hard"),
        ("She ___ (can) speak three languages.", "can", "can 表示能力",
         "能力 can", "easy"),
        ("We ___ (should) protect the environment.", "should", "should 表建议",
         "建议 should", "easy"),
        ("It ___ (may) be true.", "might", "might 可能性比 may 更小",
         "更不确定用 might", "medium"),
        ("___ (will) you please open the window?", "Will", "Will you 请你...表请求",
         "请求 Will you", "medium"),
        ("I ___ (need) not go there.", "need", "need 作情态动词表不必",
         "不必 need not/don't need to", "medium"),
        ("He ___ (used) to play football.", "used", "used to do 过去常常",
         "过去习惯 used to do", "hard"),
    ]

    for sent, ans, hint, exp, diff in modal_fills:
        qs.append(make_fill(nid(), "verb", diff, sent, hint, ans, exp,
            [sent.replace(f"({sent.split('(')[1].split(')')[0]})", f"<strong>{ans}</strong>")],
            ""))

    return qs


def gerund_infinitive_data():
    """动词+不定式 vs 动词+动名词"""
    qs = []

    # === 动词+doing (动名词) ===
    doing_verbs = [
        ("enjoy", "enjoy reading", "enjoy to read", "enjoy + doing 表示喜欢做某事",
         ["I enjoy <strong>listening</strong> to music.（我喜欢听音乐。）",
          "Do you enjoy swimming?（你喜欢游泳吗？）"],
         "喜欢 enjoy doing"),
        ("finish", "finished doing", "finished to do", "finish + doing 表示完成某事",
         ["I finished <strong>doing</strong> my homework.（我做完了作业。）",
          "Has she finished reading?（她读完了吗？）"],
         "完成 finish doing"),
        ("practice", "practices speaking", "practices to speak", "practice + doing 表示练习",
         ["You should practice speaking <strong>English</strong>.（你应该练习说英语。）",
          "He practices playing piano every day.（他每天练习弹钢琴。）"],
         "练习 practice doing"),
        ("mind", "mind opening", "mind to open", "mind + doing 表示介意做某事",
         ["Would you mind <strong>opening</strong> the window?（你介意开窗吗？）",
          "I don't mind waiting.（我不介意等。）"],
         "介意 mind doing"),
        ("avoid", "avoid making", "avoid to make", "avoid + doing 表示避免做某事",
         ["Try to avoid making <strong>mistakes</strong>.（尽量避免犯错误。）",
          "He avoided answering my question.（他回避了我的问题。）"],
         "避免 avoid doing"),
        ("suggest", "suggested going", "suggested to go", "suggest + doing 表示建议做某事",
         ["I suggested going <strong>by bus</strong>.（我建议坐公交去。）",
          "The doctor suggested taking a rest.（医生建议休息一下。）"],
         "建议 suggest doing"),
        ("consider", "consider buying", "consider to buy", "consider + doing 表示考虑做某事",
         ["I'm considering changing <strong>my job</strong>.（我在考虑换工作。）",
          "Have you considered studying abroad?（你考虑过出国留学吗？）"],
         "考虑 consider doing"),
        ("keep", "keeps asking", "keeps to ask", "keep + doing 表示持续做某事",
         ["He kept asking <strong>questions</strong>.（他不停地问问题。）",
          "Keep trying!（继续努力！）"],
         "持续 keep doing"),
        ("give up", "gave up smoking", "gave up to smoke", "give up + doing 表示放弃做某事",
         ["You should give up smoking.（你应该戒烟。）",
          "Don't give up trying.（不要放弃尝试。）"],
         "放弃 give up doing"),
        ("imagine", "imagined being", "imagined to be", "imagine + doing 表示想象做某事",
         ["I can't imagine living alone.（我无法想象独自生活。）",
          "Imagine winning the lottery!（想象一下中彩票！）"],
         "想象 imagine doing"),
        ("miss", "misses seeing", "misses to see", "miss + doing 表示怀念/错过做某事",
         ["I miss seeing you.（我想念见到你的时光。）",
          "He missed catching the bus.（他错过了赶公交车。）"],
         "怀念/错过 miss doing"),
        ("can't help", "couldn't help laughing", "couldn't help to laugh", "can't help + doing 情不自禁",
         ["I couldn't help laughing.（我忍不住笑了起来。）",
          "She can't help worrying about him.（她忍不住担心他。）"],
         "忍不住 can't help doing"),
        ("feel like", "felt like going", "felt like to go", "feel like + doing 想要做",
         ["I feel like having a drink.（我想喝一杯。）",
          "Do you feel like going out?（你想出去吗？）"],
         "想要 feel like doing"),
        ("look forward to", "looking forward to seeing", "looking forward to see", "look forward to + doing 期待",
         ["I look forward to hearing from you.（期待收到你的来信。）",
          "We're looking forward to visiting Beijing.（我们期待访问北京。）"],
         "期待 look forward to doing"),
        ("be used to", "is used to getting", "is used to to get", "be used to + doing 习惯于",
         ["I'm used to getting up early.（我习惯了早起。）",
          "He is used to living alone.（他习惯了独居。）"],
         "习惯 be used to doing"),
        ("spend...(in)", "spent...(in) cleaning", "spent to clean", "spend time (in) doing",
         ["She spent two hours cooking.（她花了两个小时做饭。）",
          "Don't waste time watching TV.（别浪费时间看电视。）"],
         "花时间 spend (in) doing"),
        ("busy", "busy preparing", "busy to prepare", "be busy doing 忙于做",
         ["He's busy writing a report.（他正忙着写报告。）",
          "I'm busy doing my homework.（我正忙着做作业。）"],
         "忙于 busy doing"),
        ("have fun/trouble/difficulty", "had fun learning", "had fun to learn", "have fun/trouble (in) doing",
         ["We had fun playing games.（我们玩得很开心。）",
          "I had trouble solving the problem.（我在解决这个问题时有困难。）"],
         "开心/有困难 have fun/trouble doing"),
    ]

    for verb, correct, wrong, exp, exs, mn in doing_verbs:
        d = "easy" if verb in ("enjoy","finish","practice","mind","keep","give up") else \
            "medium" if verb in ("avoid","suggest","consider","miss","can't help","feel like") else "hard"
        o, ai = shuffle_options([wrong, correct, correct.replace("ing", "to ")[:len(correct)], verb+"ing"], 1)
        # 重新构造选项
        base = verb.split()[-1] if " " in verb else verb
        alt1 = wrong
        alt2 = base + " to do" if "(" not in verb else verb.split("(")[0] + " to do"
        alt3 = base + " do"
        opts = [alt1, correct, alt2, alt3]
        opts = list(set(opts))  # 去重
        if len(opts) < 4:
            opts.append(base + " for doing")
        o2, ai2 = shuffle_options(opts[:4], opts.index(correct) if correct in opts else 0)
        # 找到正确答案的位置
        for i, opt in enumerate(o2):
            if "ing" in opt and "to " not in opt and verb.split()[0][:3] in opt.lower()[:5]:
                ai2 = i
                break
        qs.append(make_mc(nid(), "verb", d,
            f"Choose the correct form: She {correct.replace(verb+' ','') if verb+' ' in correct else '___'} every day."
            if "___" not in (correct.replace(verb+' ','') if verb+' ' in correct else '___')
            else f"Complete: She ___ every day.",
            f"{verb} 的用法", o2, ai2, exp, exs, mn))

    # === 动词+to do (不定式) ===
    todo_verbs = [
        ("want", "want to learn", "want learning", "want + to do 表示想要做",
         ["I want to <strong>go</strong> home.（我想回家。）",
          "She wants to buy a new phone.（她想买个新手机。）"],
         "想要 want to do"),
        ("hope", "hope to see", "hope seeing", "hope + to do 表示希望做",
         ["I hope to visit Beijing.（我希望去北京参观。）",
          "We hope to win the game.（我们希望赢得比赛。）"],
         "希望 hope to do"),
        ("decide", "decided to study", "decided studying", "decide + to do 决定做",
         ["She decided to become a doctor.（她决定当医生。）",
          "We decided to leave early.（我们决定早点离开。）"],
         "决定 decide to do"),
        ("plan", "plans to travel", "plans travelling", "plan + to do 计划做",
         ["I plan to study abroad.（我计划出国留学。）",
          "He plans to visit his grandparents.（他计划去看望祖父母。）"],
         "计划 plan to do"),
        ("promise", "promised to help", "promised helping", "promise + to do 承诺做",
         ["I promised to call you back.（我答应给你回电话。）",
          "She promised to keep the secret.（她承诺保守秘密。）"],
         "承诺 promise to do"),
        ("offer", "offered to drive", "offered driving", "offer + to do 主动提出做",
         ["He offered to carry my bag.（他主动提出帮我拿包。）",
          "She offered to help me with English.（她主动提出帮我学英语。）"],
         "主动提出 offer to do"),
        ("refuse", "refused to accept", "refused accepting", "refuse + to do 拒绝做",
         ["He refused to answer.（他拒绝回答。）",
          "She refused to go with us.（她拒绝和我们一起去。）"],
         "拒绝 refuse to do"),
        ("agree", "agreed to join", "agreed joining", "agree + to do 同意做",
         ["I agreed to help him.（我同意帮他。）",
          "They agreed to meet at 7.（他们约定7点见面。）"],
         "同意 agree to do"),
        ("would like", "would like to try", "would like trying", "would like + to do 想要",
         ["I would like to drink some water.（我想喝些水。）",
          "Would you like to join us?（你要加入我们吗？）"],
         "想要 would like to do"),
        ("ask/tell", "asked me to go", "asked me going", "ask/tell sb. to do 叫某人做",
         ["My mom asked me to clean my room.（妈妈让我打扫房间。）",
          "The teacher told us to be quiet.（老师让我们安静。）"],
         "叫某人做 ask/tell sb. to do"),
        ("it takes sb.", "It took me an hour to finish", "It took me finishing", "It takes sb. time to do",
         ["It took me two hours to do the homework.（做作业花了我两小时。）",
          "It will take us three days.（我们将花三天时间。）"],
         "花时间 It takes sb. time to do"),
        ("too...to", "too young to drive", "too young driving", "too...to... 太...而不能...",
         ["He is too young to drive.（他太年轻了，还不能开车。）",
          "It's too heavy to carry.（它太重了，搬不动。）"],
         "太...不能 too...to do"),
        ("enough...to", "brave enough to try", "enough brave trying", "...enough to do 足够...可以做",
         ["He is old enough to go to school.（他足够大了，可以去上学了。）",
          "It's warm enough to go out.（天气够暖和了，可以出去了。）"],
         "足够...能 enough to do"),
    ]

    for verb, correct, wrong, exp, exs, mn in todo_verbs:
        d = "easy" if verb in ("want","hope","plan","decide","would like") else \
            "medium" if verb in ("promise","offer","refuse","agree","ask/tell") else "hard"
        alt1 = wrong
        alt2 = correct.replace("to ", "")
        alt3 = correct.replace("to do", "doing").replace("to go", "going")
        opts_raw = list(set([correct, alt1, alt2, alt3]))
        if len(opts_raw) >= 4:
            opts = opts_raw[:4]
        else:
            opts = opts_raw + [verb + " do"]
        o, ai = shuffle_options(opts, 0)
        for i, opt in enumerate(o):
            if "to " in opt and opt != alt1:
                ai = i
                break
        qs.append(make_mc(nid(), "verb", d,
            f"Choose the correct form: {correct}",
            f"{verb} 的用法", o, ai, exp, exs, mn))

    # === 既可+doing又可+to do (意义不同) ===
    both_verbs = [
        ("remember", "remember to lock", "remember locking",
         "remember to do 记得去做（还没做）；remember doing 记得做过（已做）",
         ["Remember to lock the door.（记得去锁门——还没锁。）",
          "I remember locking the door.（我记得锁了门——已经锁了。）"],
         "未做 remember to do; 已做 remember doing"),
        ("forget", "forget to mail", "forget mailing",
         "forget to do 忘记去做（没做）；forget doing 忘记做过（做了但忘了）",
         ["Don't forget to close the window.（别忘了关窗——还没关。）",
          "I forgot turning off the light.（我忘了是否关灯了。）"],
         "未做 forget to do; 已忘 forget doing"),
        ("stop", "stopped to rest", "stopped resting",
         "stop to do 停下来去做（另一件事）；stop doing 停止做（当前的事）",
         ["He stopped to have a rest.（他停下来休息了一下。）",
          "Stop talking!（停止讲话！）"],
         "停下(去做别的) stop to do; 停止 stop doing"),
        ("try", "try to solve", "try solving",
         "try to do 尽力做；try doing 尝试做（看看效果如何）",
         ["I'll try to finish it tonight.（我今晚尽力完成它。）",
          "Try adding more salt.（试试多加点盐。）"],
         "尽力 try to do; 尝试 try doing"),
        ("go on", "went on to read", "went on reading",
         "go on to do 接着做另一件事；go on doing 继续做同一件事",
         ["After finishing math, he went on to read English.（做完数学后，他接着读英语。）",
          "Go on reading, please.（请继续读。）"],
         "换事 go on to do; 继续 go on doing"),
        ("mean", "means to give up", "means giving up",
         "mean to do 打算做；meaning doing 意味着做",
         ["I mean to go there tomorrow.（我打算明天去那里。）",
          "Missing the train means waiting for an hour.（错过火车意味着要等一小时。）"],
         "打算 mean to do; 意味着 mean doing"),
    ]

    for verb, correct_a, correct_b, exp, exs, mn in both_verbs:
        for correct, scenario in [(correct_a, "to do"), (correct_b, "doing")]:
            other = correct_b if correct == correct_a else correct_a
            o, ai = shuffle_options([other, correct, verb + " do", verb + " ing"], 1)
            qs.append(make_mc(nid(), "verb", "hard",
                f"\"I ___\" — which form fits: \"{scenario}\"?",
                f"{verb} + {scenario}",
                o, ai, exp, exs, mn))

    return qs


# ============================================================
# 第二部分: tenses.json - 时态题库 (目标 500 题)
# ============================================================

def gen_simple_present():
    """一般现在时"""
    qs = []
    data = [
        ("She ___ to school every day.", "goes", ["go", "going", "gone"],
         "一般现在时，she 是第三人称单数，动词加 -es。",
         ["She goes to school by bus.（她乘公交车上学。）",
          "He goes to bed at 9 PM.（他晚上9点上床睡觉。）"],
         "第三人称单数加 -s/-es"),
        ("Water ___ at 100 degrees Celsius.", "boils", ["boil", "boiled", "boiling"],
         "客观真理/科学事实用一般现在时。",
         ["The earth moves around the sun.（地球绕太阳转。）",
          "Light travels faster than sound.（光速比声速快。）"],
         "客观真理 一般现在时"),
        ("The train ___ at 8:00 tomorrow.", "leaves", ["will leave", "is leaving", "left"],
         "按时刻表/日程表的将来安排用一般现在时表示将来。",
         ["The plane takes off at 10 AM.（飞机上午10点起飞。）",
          "School starts on September 1st.（学校9月1日开学。）"],
         "时刻表将来用一般现在时"),
        ("I ___ apples but I don't like bananas.", "like", ["likes", "liking", "liked"],
         "第一人称复数/第二人称/复数主语用动词原形。",
         ["They like playing basketball.（他们喜欢打篮球。）",
          "We love our country.（我们热爱我们的祖国。）"],
         "非第三人称用原形"),
        ("He often ___ TV after dinner.", "watches", ["watch", "watching", "watched"],
         "频率副词 often 后面用一般现在时；he 第三人称单数。",
         ["She always gets up early.（她总是早起。）",
          "He never eats breakfast.（他从不吃早饭。）"],
         "频率副词+一般现在时"),
        ("___ you like Chinese food?", "Do", ["Does", "Are", "Is"],
         "一般现在时的疑问句，主语 you 用助动词 Do。",
         ["Does she like cats?（她喜欢猫吗？）",
          "Do they live here?（他们住在这儿吗？）"],
         "疑问句 Do/Does + 主语 + 原形"),
        ("There ___ a library near my house.", "is", ["are", "has", "have"],
         "there be 结构，library 是单数，用 is。",
         ["There are many students in the classroom.（教室里有很多学生。）",
          "There is a park nearby.（附近有一个公园。）"],
         "There be 就近原则"),
        ("My father ___ newspapers every morning.", "reads", ["read", "reading", "readed"],
         "father 第三人称单数，read 的单三形式 reads（读音 /ri:dz/）。",
         ["She reads books before sleep.（她睡前看书。）",
          "He reads the news every day.（他每天看新闻。）"],
         "read 的单三是 reads"),
        ("The sun ___ in the east.", "rises", ["raise", "rised", "raising"],
         "太阳东升是客观真理，rise 的第三人称单数是 rises。",
         ["The river flows into the sea.（河流汇入大海。）"],
         "客观真理 rises"),
        ("___ your brother play football?", "Does", ["Do", "Is", "Are"],
         "brother 第三人称单数疑问句用 Does。",
         ["Does it rain a lot here?（这儿雨多吗？）"],
         "第三人称疑问 Does"),
        ("She doesn't ___ meat.", "eat", ["eats", "eating", "ate"],
         "doesn't 后面接动词原形。",
         ["He doesn't like spicy food.（他不喜欢辣的食物。）",
          "They don't live in Beijing.（他们不住在北京。）"],
         "don't/doesn't + 原形"),
        ("Tom and Jerry ___ good friends.", "are", ["is", "am", "be"],
         "Tom and Jerry 是复数，be 动词用 are。",
         ["We are students.（我们是学生。）",
          "They are brothers.（他们是兄弟。）"],
         "复数用 are"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_present", "easy", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_present_continuous():
    """现在进行时"""
    qs = []
    data = [
        ("Look! It ___ now.", "is raining", ["rains", "rained", "rain"],
         "Look!/Listen! 提示动作正在进行，用现在进行时 be doing。",
         ["Listen! Someone is singing.（听！有人在唱歌。）",
          "Look! The kids are playing soccer.（看！孩子们在踢足球。）"],
         "Look/Listen → 进行时"),
        ("I ___ my homework at the moment.", "am doing", ["do", "did", "done"],
         "at the moment/right now/now 提示现在进行时。",
         ["She is working on a project now.（她现在正在做一个项目。）",
          "We are having dinner right now.（我们现在正吃晚餐呢。）"],
         "now/at the moment → be doing"),
        ("We ___ to Beijing next week.", "are going", ["go", "went", "will go"],
         "表示按计划安排好的将来动作（位移类动词），可用现在进行时表将来。",
         ["I'm meeting Tom tomorrow.（我明天要见汤姆。）",
          "They are flying to London next month.（他们下个月飞往伦敦。）"],
         "计划好的将来 → be doing"),
        ("She ___ a letter these days.", "is writing", ["writes", "wrote", "writing"],
         "these days/these weeks 表示现阶段一直在进行的动作。",
         ["I'm reading a novel these days.（这些天我正在读一本小说。）",
          "He's learning French these months.（这几个月他在学法语。）"],
         "these days → 现在进行时"),
        ("Be quiet! The baby ___.", "is sleeping", ["sleeps", "slept", "sleep"],
         "Be quiet! 说明此刻正在发生的动作。",
         ["Please be quiet! The teacher is coming.（请安静！老师来了。）"],
         "Be quiet → 正在发生"),
        ("Why ___ you ___ so fast?", "are;running", ["do;run", "are;run", "does;run"],
         "现在进行时的特殊疑问句：be + 主语 + doing?",
         ["What are you doing?（你在做什么？）",
          "Where is she going?（她要去哪里？）"],
         "疑问句 be + 主语 + doing"),
        ("He isn't ___ attention in class.", "paying", ["pay", "paid", "pays"],
         "现在进行时的否定：be not doing。",
         ["They aren't watching TV.（他们没在看电视。）",
          "I'm not feeling well today.（我今天感觉不舒服。）"],
         "否定 be not doing"),
        ("More and more people ___ cars nowadays.", "are buying", ["buy", "bought", "buys"],
         "nowadays（如今）配合进行时表示当前的趋势。",
         ["The climate is getting warmer.（气候在变暖。）",
          "Technology is developing fast.（科技发展很快。）"],
         "nowadays → 进行时表趋势"),
        ("I ___ tired recently.", "am feeling", ["feel", "felt", "feeling"],
         "feel, smell, taste 等感官动词可用进行时表示暂时的状态。",
         ["I'm feeling hungry.（我现在觉得饿。）",
          "How are you feeling today?（你今天感觉怎么样？）"],
         "感官动词可表暂时状态"),
        ("___ they ___ for the exam?", "Are;preparing", ["Do;prepare", "Are;prepare", "Does;prepare"],
         "they 复数，现在进行时疑问句用 Are...doing?",
         ["Is he coming to the party?（他会来派对吗？）"],
         "复数 Are + doing"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        if ";" in ans:
            parts = ans.split(";")
            combined = " ".join(parts)
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_present", "medium", new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_present", "easy", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_present_perfect():
    """现在完成时"""
    qs = []
    data = [
        ("I ___ already ___ my homework.", "have;finished", ["have;finish", "finished;", "did;finish"],
         "already 常与现在完成时连用，表示已完成。",
         ["I have already eaten breakfast.（我已经吃过早饭了。）",
          "She has already left.（她已经离开了。）"],
         "already → 现在完成时"),
        ("She hasn't ___ the news ___.", "heard;yet", ["heard;already", "hear;yet", "heard;just"],
         "yet 常用于否定句和疑问句末尾，与现在完成时连用。",
         ["Have you finished yet?（你完成了吗？）",
          "I haven't read the book yet.（我还没读过这本书。）"],
         "yet → 现在完成时（否/疑）"),
        ("___ you ever ___ to Japan?", "Have;been", ["Do;go", "Did;go", "Are;going"],
         "ever 曾经（用于现在完成时疑问句）。",
         ["Have you ever seen a tiger?（你见过老虎吗？）",
          "Has she ever been abroad?（她出过国吗？）"],
         "ever → Have/Has ... ever done"),
        ("I ___ just ___ my meal.", "have;had", ["have;have", "did;have", "do;have"],
         "just 刚刚（放在 have/has 和过去分词之间）。",
         ["She has just arrived.（她刚到。）",
          "They have just finished the meeting.（他们刚结束会议。）"],
         "just → have/has just done"),
        ("He ___ in this city since 2010.", "has lived", ["has lived", "lives", "is living"],
         "since + 过去时间点，用现在完成时。",
         ["I've known her since childhood.（我从儿时就认识她。）",
          "We've been friends since primary school.（我们从小学起就是朋友。）"],
         "since + 时间点 → 完成时"),
        ("They ___ here for ten years.", "have been", ["are", "were", "have"],
         "for + 时间段，用现在完成时表示持续到现在。",
         ["I have studied English for 5 years.（我学英语已经5年了。）",
          "She has worked here for a long time.（她在这里工作很久了。）"],
         "for + 时间段 → 完成时"),
        ("This is the best film I ___.", "have ever seen", ["ever saw", "ever see", "had ever seen"],
         "最高级 + have/has ever done（完成时）是常见句型。",
         ["It's the most delicious food I have ever tasted.（这是我最尝过最好吃的食物。）"],
         "最高级 + have ever done"),
        ("It is the first time I ___.", "have visited", ["visit", "visited", "am visiting"],
         "It is the first/second/time + have/has done（完成时）。",
         ["It's the second time I have made this mistake.（这是我第二次犯这个错误。）"],
         "第N次 + have done"),
        ("She ___ to Paris. (She is still there)", "has gone", ["has been", "was", "is"],
         "has gone to 去了（还没回来）；has been to 去过（已回）。",
         ["I have been to Beijing twice.（我去过北京两次。）",
          "Where has he gone?他去哪儿了？（不在说话处）"],
         "去了没回 has gone to; 去过已回 has been to"),
        ("I ___ to Shanghai twice.", "have been", ["have gone", "was", "went"],
         "twice 表示经历，用 have been to。",
         ["Have you ever been to the Great Wall?（你去过长城吗？）"],
         "次数 + have been to"),
        ("So far, we ___ 500 words.", "have learned", ["learn", "learned", "are learning"],
         "so far/up to now 迄今为止，标志现在完成时。",
         ["Up to now, everything goes well.（到目前为止，一切顺利。）",
          "So far I haven't received any reply.（至今我没收到任何回复。）"],
         "so far → 现在完成时"),
        ("The door ___. Someone must have opened it.", "has been opened", ["opened", "is opening", "opens"],
         "现在完成时表示过去的动作对现在造成的影响/结果。",
         ["I've lost my key.（我的钥匙丢了。——结果：进不去）",
          "She has bought a new car.（她买了辆新车。——结果：有了新车）"],
         "过去影响现在 → 完成时"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        if ";" in ans:
            combined = ans.replace(";", " ")
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_present", "medium", new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_present", "medium", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_present_perf_cont():
    """现在完成进行时"""
    qs = []
    data = [
        ("I ___ for two hours.", "have been waiting", ["have waited", "am waiting", "wait"],
         "have/has been doing 表示动作从过去开始一直持续到现在还在进行。",
         ["It has been raining all day.（下了一整天雨了。）",
          "She has been studying since morning.（她从早上就在学习了。）"],
         "一直进行 have/has been doing"),
        ("She is tired because she ___ all day.", "has been working", ["has worked", "is working", "worked"],
         "现在完成进行时常用来解释原因（为什么累、脏等）。",
         ["Your eyes are red. Have you been crying?（你眼睛红了，哭了吗？）",
          "He is sweating because he has been running.（他满头大汗，因为一直在跑。）"],
         "解释原因 → 完成进行时"),
        ("How long ___ you ___ English?", "have;been;learning", ["have;learned", "do;learn", "did;learn"],
         "how long 询问持续时间，用完成进行时强调过程。",
         ["How long have you been living here?（你在这里住了多久了？）",
          "How long has she been practicing?（她练习多久了？）"],
         "How long → have/has been doing"),
        ("I ___ really hard recently.", "have been studying", ["have studied", "am studying", "studied"],
         "recently/lately 配合完成进行时表示近期持续的动作。",
         ["I've been feeling a bit unwell lately.（最近我感觉有点不舒服。）"],
         "recently/lately → 完成进行时"),
        ("They ___ on this project since January.", "have been working", ["work", "worked", "are working"],
         "since + 时间点 + 持续性的动作，用现在完成进行时。",
         ["It has been snowing since last night.（从昨晚起就一直在下雪。）"],
         "since + 持续动作 → 完成进行时"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        if ";" in ans:
            combined = ans.replace(";", " ")
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___ ___", "___").replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_present", "hard", new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_present", "hard", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_simple_past():
    """一般过去时"""
    qs = []
    data = [
        ("I ___ to the cinema yesterday.", "went", ["go", "have gone", "was going"],
         "yesterday 是明确的过去时间标志，用一般过去时。",
         ["She came back last night.（她昨晚回来的。）",
          "We visited the museum last week.（上周我们去参观了博物馆。）"],
         "yesterday/last... → 过去时"),
        ("___ you ___ your homework last night?", "Did;finish", ["Do;finish", "Have;finished", "Were;finishing"],
         "last night 是过去的时间，一般过去时疑问句用 Did + 原形。",
         ["Did you watch the game yesterday?（昨天你看比赛了吗？）",
          "When did she arrive?（她什么时候到的？）"],
         "Did + 原形 (过去时疑问)"),
        ("He ___ born in 2005.", "was", ["is", "has been", "borned"],
         "be born 出生（be 动词的过去式）。",
         ["When were you born?（你什么时候出生的？）",
          "She was born in Shanghai.（她出生在上海。）"],
         "be born 用 was/were"),
        ("I ___ my keys this morning.", "lost", ["have lost", "lose", "was losing"],
         "this morning 如果已经过去了（下午或晚上说），用一般过去时。",
         ["I met an old friend this afternoon.（今天下午我遇到了一位老朋友。）"],
         "已过的 this morning/afternoon → 过去时"),
        ("She said she ___ busy.", "was", ["is", "has been", "being"],
         "主句是一般过去时，从句也常用过去的某种时态（时态一致）。",
         ["He told me he liked music.（他告诉我他喜欢音乐。）",
          "I thought he was at home.（我以为他在家。）"],
         "主句过去 → 从句也倾向过去"),
        ("We ___ fun at the party last night.", "had", ["have", "are having", "have had"],
         "last night 标志过去时；have fun 玩得开心。",
         ["They had a great time yesterday.（昨天他们玩得很开心。）"],
         "have fun 过去式 had fun"),
        ("The weather ___ nice last week.", "was", ["is", "has been", "were"],
         "last week 过去时间，weather 不可数用 was。",
         ["It was cold yesterday.（昨天很冷。）",
          "The food was delicious.（食物很好吃。）"],
         "过去状态 was/were"),
        ("I didn't ___ anyone at the party.", "see", ["saw", "seen", "seeing"],
         "didn't 后面接动词原形。",
         ["She didn't come to school yesterday.（她昨天没来上学。）",
          "They didn't watch the movie.（他们没看那部电影。）"],
         "didn't + 原形"),
        ("When I was young, I ___ swimming every summer.", "went", ["go", "have gone", "was going"],
         "when I was young 回忆过去的状态/习惯。",
         ["When I was a child, I lived in the countryside.（小时候我住在乡下。）"],
         "回忆过去习惯 → 一般过去时"),
        ("He suddenly ___ that he left his wallet at home.", "realized", ["realizes", "has realized", "realizing"],
         "叙述过去发生的事件序列。",
         ["She opened the door and found nobody.（她打开门发现没人。）",
          "He stood up and walked away.（他站起来走了出去。）"],
         "叙述过去事件 → 一般过去时"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        if ";" in ans:
            combined = ans.replace(";", " ")
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_past", "easy", new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_past", "easy", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_past_continuous():
    """过去进行时"""
    qs = []
    data = [
        ("I ___ TV when you called me.", "was watching", ["watched", "am watching", "watch"],
         "when 引导的一般过去时从句，主句常用过去进行时（长动作被短动作打断）。",
         ["I was cooking when the phone rang.（电话响的时候我在做饭。）",
          "She was reading when I came in.（我进来的时候她在读书。）"],
         "长动作进行中 when 短动作发生"),
        ("What ___ you ___ at 8 PM last night?", "were;doing", ["did;do", "do;do", "are;doing"],
         "特定过去时刻正在进行的动作，用过去进行时。",
         ["What was he doing at this time yesterday?（这个时候昨天他在做什么？）"],
         "特定过去时刻 → was/were doing"),
        ("While I ___ , it started to rain.", "was walking", ["walked", "walk", "am walking"],
         "while 引导两个同时进行的延续性动作，前后都用过去进行时。",
         ["While Mom was cooking, Dad was reading.（妈妈做饭的时候爸爸在看书。）",
          "While they were talking, it began to rain.（他们聊天的时候开始下雨了。）"],
         "while → 两个都在进行"),
        ("They ___ football from 3 to 5 yesterday.", "were playing", ["played", "play", "are playing"],
         "from...to... 表示过去某段时间内持续的动作。",
         ["I was sleeping from 9 to 11 last night.（昨晚9点到11点我在睡觉。）"],
         "过去时间段内 → was/were doing"),
        ("She wasn't ___ attention to the teacher.", "paying", ["pay", "paid", "pays"],
         "过去进行时的否定形式：was/were not doing。",
         ["They weren't listening to the music.（他们没在听音乐。）"],
         "否定 was/were not doing"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        if ";" in ans:
            combined = ans.replace(";", " ")
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_past", "medium", new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_past", "medium", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_past_perfect():
    """过去完成时"""
    qs = []
    data = [
        ("When I got to the station, the train ___.", "had left", ["left", "has left", "leaves"],
         "\"过去的过去\"——火车在我到站之前就已经离开了。",
         ["When she arrived, the meeting had already started.（她到达时会议已经开始了。）",
          "By the time I woke up, my brother had left.（到我醒来时，哥哥已经走了。）"],
         "过去的过去 had done"),
        ("She said she ___ the film before.", "had seen", ["saw", "has seen", "sees"],
         "间接引语中，\"过去的过去\"用过去完成时。",
         ["He told me he had finished his homework.（他告诉我他已经做完了作业。）"],
         "间接引语中的「过去的过去」"),
        ("By the end of last year, we ___ 1000 English words.", "had learned", ["have learned", "learned", "learn"],
         "by the end of + 过去时间，截止到那个过去时间点之前完成的动作。",
         ["By the age of 10, he could swim.（到10岁时他就会游泳了。）",
          "By last Friday, they had completed the project.（到上周五他们已经完成了项目。）"],
         "by + 过去时间 → had done"),
        ("I didn't go to the film because I ___ it.", "had seen", ["have seen", "saw", "see"],
         "原因状语从句中先于主句动作完成的动作用过去完成时。",
         ["She was tired because she had worked all day.（她累了，因为她工作了一整天。）"],
         "原因发生在主句之前 → had done"),
        ("He wished he ___ more careful.", "had been", ["was", "has been", "is"],
         "wish/if only + 过去完成时，表示与过去事实相反的愿望。",
         ["If only I had known earlier!（我要是早知道就好了！）",
          "I wish I had studied harder.（我当时要是更努力学就好了。）"],
         "wish + had done（与过去相反的愿望）"),
        ("It was the first time I ___. such a beautiful place.", "had visited", ["have visited", "visited", "visit"],
         "It was + 序数词 + time + 主语 + had done。",
         ["It was the second time she had made that mistake.（那是她第二次犯那个错误了。）"],
         "It was 第N次 had done"),
        ("Hardly had I ___ home when it rained.", "reached", ["reach", "reached", "have reached"],
         "Hardly had + 主语 + done...when...（一...就...，倒装句）。",
         ["No sooner had he left than she arrived.（他一走她就到了。）"],
         "Hardly had done... when..."),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_past", "hard", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_simple_future():
    """一般将来时"""
    qs = []
    data = [
        ("I think it ___ rain tomorrow.", "will", ["shall", "is going to", "going to"],
         "will 表示主观猜测/判断（我认为会...）。",
         ["I think she will like the gift.（我觉得她会喜欢这个礼物。）",
          "Maybe it will snow tomorrow.（也许明天会下雪。）"],
         "主观猜测 will"),
        ("Look at those dark clouds! It ___ rain.", "is going to", ["will", "shall", "does"],
         "有迹象表明即将发生的事用 be going to。",
         ["Watch out! You're going to fall.（小心！你要摔倒了。）",
          "I feel terrible. I'm going to be sick.（感觉很难受，我要吐了。）"],
         "有迹象 → be going to"),
        ("___ you free tomorrow evening?", "Will", ["Do", "Are", "Shall"],
         "询问对方意图/意愿用 Will you...?",
         ["Will you join us for dinner?（你和我们一起吃晚饭好吗？）",
          "Will you please open the window?（请你打开窗户好吗？）"],
         "询问意愿 Will you...?"),
        ("We ___ start the meeting at 9 AM.", "are going to", ["will", "shall", "do"],
         "事先计划/安排好的事情用 be going to。",
         ["They are going to get married next month.（他们下个月要结婚。）",
          "I'm going to visit my grandma this weekend.（这个周末我打算去看望奶奶。）"],
         "计划安排 → be going to"),
        ("The train ___ leave in five minutes.", "is about to", ["will", "is going to", "shall"],
         "be about to do 即将、马上（不与具体时间连用）。",
         ["The show is about to begin.（演出马上就要开始了。）",
          "Hurry up! The bus is about to leave.（快点！公交车马上就要开了。）"],
         "马上 → be about to do"),
        ("I ___ be 18 years old next birthday.", "will", ["am going to", "am", "shall"],
         "单纯表示将来的事实/状态用 will。",
         ["Tomorrow will be Monday.（明天是周一。）",
          "One day, humans will live on Mars.（终有一天人类将在火星生活。）"],
         "将来事实 will"),
        ("___ we go for a walk?", "Shall", ["Will", "Do", "Are"],
         "Shall we...? 第一人称疑问句表提议。",
         ["Shall we dance?（我们要跳舞吗？）",
          "Shall I open the window?（要我开窗吗？）"],
         "提议 Shall we/I...?"),
        ("Be quick! The film ___.", "is starting", ["starts", "will start", "started"],
         "位移类动词（come/go/start/arrive/leave）的现在进行时可表按计划的将来。",
         ["The bus is coming.（公交车来了/要来了。）",
          "We are flying to Tokyo next week.（下周我们要飞往东京。）"],
         "位移动词 be doing = 将来计划"),
        ("I promise I ___ tell anyone.", "won't", ["don't", "am not going to", "shan't"],
         "will not = won't 表示承诺/决心。",
         ["I'll never give up!（我绝不会放弃！）",
          "I will help you.（我会帮你。）"],
         "承诺/决心 won't/will"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_future", "medium", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_future_continuous():
    """将来进行时"""
    qs = []
    data = [
        ("This time tomorrow, I ___ on the beach.", "will be lying", ["will lie", "am lying", "will have lain"],
         "将来某一时刻正在进行的动作：will be doing。",
         ["At 8 PM tonight, I will be watching TV.（今晚8点我将正在看电视。）",
          "This time next year, we will be traveling in Europe.（明年的这个时候我们将在欧洲旅行。）"],
         "将来某刻 will be doing"),
        ("Don't call me at 9—I ___ then.", "will be sleeping", ["sleep", "will sleep", "am sleeping"],
         "将来某个时间预计正在做的事情。",
         ["Will you be using the computer tonight?（你今晚要用电脑吗？）"],
         "将来预计在做 will be doing"),
        ("She ___ for you at the airport at 3 PM.", "will be waiting", ["waits", "will wait", "is waiting"],
         "约定好的将来某个时刻的动作。",
         ["The band will be performing at 7 o'clock.（乐队将在7点演出。）"],
         "约定时刻 will be doing"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_future", "hard", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_future_perfect():
    """将来完成时"""
    qs = []
    data = [
        ("By next Friday, we ___ the project.", "will have finished", ["will finish", "have finished", "finished"],
         "by + 将来完成时：will have done（到将来某时将已完成）。",
         ["By 2030, I will have graduated.（到2030年我就毕业了。）",
          "By the time you get home, I will have cooked dinner.（到你回家时，我就做好晚饭了。）"],
         "by + 将来时间 → will have done"),
        ("She ___ this book by tomorrow.", "will have read", ["will read", "has read", "reads"],
         "表示到将来某时会已经完成的动作。",
         ["Will you have finished by then?（到时候你会完成吗？）"],
         "将来将已完成 will have done"),
        ("Before he turns 30, he ___ ten countries.", "will have visited", ["will visit", "has visited", "visited"],
         "before + 将来时间 → will have done。",
         ["I hope I will have learned 5000 words by next year.（我希望明年之前学会5000个单词。）"],
         "before 将来 → will have done"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_future", "hard", sent, "", o, ai, exp, exs, mn))

    return qs


def gen_conditionals():
    """条件句时态搭配"""
    qs = []
    data = [
        # 真实条件句（第一类：主将从现）
        ("If it ___ tomorrow, we will stay at home.", "rains", ["will rain", "rain", "is raining"],
         "真实条件句：if 从句用一般现在时，主句用一般将来时（主将从现）。",
         ["If you work hard, you will succeed.（如果你努力，你就会成功。）",
          "If it doesn't rain, we'll go hiking.（如果不下雨，我们就去远足。）"],
         "主句将来 if 从句现在"),
        ("Unless you ___ hard, you won't pass.", "study", ["will study", "studied", "are studying"],
         "unless (= if not) 引导的条件句也遵循「主将从现」。",
         ["Unless you hurry, you'll miss the bus.（除非你快点，否则你会错过公交车。）"],
         "unless 也遵循主将从现"),
        # 第二类虚拟条件句
        ("If I ___ you, I would accept the offer.", "were", ["am", "was", "had been"],
         "与现在事实相反的虚拟条件句：if + 一般过去时(be 用 were)，主句 would + 原形。",
         ["If I were a bird, I would fly to you.（如果我是一只鸟，我就飞向你。）",
          "If I had money, I would buy a car.（如果我有钱，我会买车。）"],
         "如果我是你 If I were you (虚拟)"),
        ("If I knew his number, I ___ him.", "would call", ["will call", "call", "called"],
         "与现在事实相反：主句用 would/could/might + 动词原形。",
         ["If she were here, she would help us.（如果她在这，她会帮助我们。）"],
         "虚拟现在 → would + 原形"),
        # 第三类虚拟条件句（与过去事实相反）
        ("If I ___ your advice, I wouldn't have failed.", "had taken", ["took", "would take", "take"],
         "与过去事实相反：if + had done, 主句 would have done。",
         ["If I had studied harder, I would have passed.（如果我更努力学习的话，我就会通过考试了。）",
          "If she had set the alarm, she wouldn't have been late.（如果她设了闹钟，她就不会迟到了。）"],
         "虚拟过去 if had done, would have done"),
        ("If he ___ more carefully, he wouldn't have made mistakes.", "had driven", ["drove", "would drive", "drives"],
         "与过去事实相反的假设。",
         ["If I had known, I would have told you.（我要是知道的话，就会告诉你了。）"],
         "与过去相反 if had done"),
        ("I wish I ___ the answer now.", "knew", ["know", "had known", "would know"],
         "wish + 一般过去时，表示与现在事实相反的愿望。",
         ["I wish I were richer.（我希望我更有钱。）",
          "I wish I could fly.（但愿我会飞。）"],
         "wish + 过去时 = 与现在相反"),
        ("I wish I ___ to the party yesterday.", "had gone", ["went", "would go", "go"],
         "wish + 过去完成时，表示与过去事实相反的愿望。",
         ["I wish I hadn't said that.（我希望我没说过那些话。）",
          "I wish I had studied medicine.（我希望我当时学了医。）"],
         "wish + had done = 与过去相反"),
        # 混合虚拟
        ("If you had listened to me, you ___ in trouble now.", "wouldn't be", ["won't be", "aren't", "weren't"],
         "混合虚拟条件句：if 从句指过去(had done)，主句指现在(would do)。",
         ["If I had saved money, I would be rich now.（如果当时存了钱，我现在就有钱了。）"],
         "混合虚拟：if过去→主句现在"),
        # 其他条件句变体
        ("___ it rain, we will cancel the trip.", "Should", ["Will", "Did", "Does"],
         "省略 if 的倒装条件句：Should it rain = If it should rain。",
         ["Had I known = If I had known（我要是早知道就好了）",
          "Were I you = If I were you（如果我是你的话）"],
         "倒装 Should/Were/Had + 主句"),
        ("I would appreciate it if you ___ me earlier.", "told", ["tell", "would tell", "had told"],
         "委婉条件句：would appreciate it if...（如果您...我将不胜感激）。",
         ["It would be better if you came earlier.（如果你早点来会更好。）"],
         "委婉 if + 过去时"),
        ("But for your help, I ___.", "would have failed", ["will fail", "fail", "failed"],
         "but for/without 要不是（含蓄条件句），暗示虚拟语气。",
         ["Without air, there would be no life.（没有空气就没有生命。）",
          "But for the rain, we would have gone out.（要不是因为下雨，我们就出去了。）"],
         "含蓄条件 but for/without → 虚拟"),
    ]

    for sent, ans, dist, exp, exs, mn in data:
        o, ai = shuffle_options(dist + [ans], 3)
        d = "hard" if "had " in ans or "would have" in ans or "wish" in sent or "Were" in ans or "Should" in ans or "But for" in sent \
            else "medium"
        qs.append(make_mc(nid(), "tense_future", d, sent, "", o, ai, exp, exs, mn))

    return qs


def gen_more_tense_questions():
    """补充更多各时态题目以达到数量要求"""
    qs = []

    # 补充一般现在时
    extra_simple_present = [
        ("The earth ___ around the sun.", "moves", ["move", "moved", "moving"], "easy",
         "客观真理用一般现在时", ["The earth <strong>moves</strong> around the sun.（地球围绕太阳转。）"],
         "客观真理 一般现在时"),
        ("My mom always ___ up at 6 AM.", "gets", ["get", "getting", "got"], "easy",
         "频率副词 + 一般现在时", ["She always <strong>gets</strong> up early.（她总是早起。）"],
         "频率副词 + 一般现在时"),
        ("Water ___ at 0°C.", "freezes", ["freeze", "frozen", "freezing"], "easy",
         "科学事实用一般现在时", ["Water <strong>freezes</strong> at zero degrees.（水在零度结冰。）"],
         "科学事实 一般现在时"),
        ("___ he like playing chess?", "Does", ["Do", "Is", "Are"], "easy",
         "第三人称疑问用 Does", ["Does she live here?（她住在这里吗？）"],
         "Does + 第三人称 + 原形"),
        ("Cats ___ mice.", "catch", ["catches", "catching", "caught"], "easy",
         "复数主语用动词原形", ["Dogs <strong>chase</strong> cats.（狗追猫。）"],
         "复数主语 → 原形"),
        ("She ___ very hard.", "studies", ["study", "studying", "studied"], "easy",
         "以y结尾的动词，辅音+y变i加es", ["He tries his best.（他尽了最大努力。）"],
         "辅音+y → studies"),
        ("He ___ glasses.", "wears", ["wear", "wearing", "weared"], "easy",
         "ear 结尾加 s", ["She wears a uniform to school.（她穿校服上学。）"],
         "wear → wears (直接+s)"),
        ("They ___ in Class 3.", "are", ["is", "am", "be"], "easy",
         "复数用 are", ["We are happy.（我们很快乐。）"],
         "复数 are"),
        ("There ___ some water in the bottle.", "is", ["are", "has", "have"], "easy",
         "water 不可数用 is", ["There is some milk in the fridge.（冰箱里有些牛奶。）"],
         "不可数 There is"),
        ("Neither of them ___ coming.", "is", ["are", "am", "be"], "medium",
         "neither/either of 作主语视为单数", ["Either of you is wrong.（你们俩有一个错了。）"],
         "neither/either of → 单数"),
    ]

    for sent, ans, dist, d, exp, exs, mn in extra_simple_present:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_present", d, sent, "", o, ai, exp, exs, mn))

    # 补充一般过去时
    extra_simple_past = [
        ("Where ___ you born?", "were", ["are", "was", "did"], "easy",
         "be born 的过去式疑问", ["Where were you born?（你出生在哪里？）"],
         "be born → was/were born"),
        ("I ___ a great time at the party.", "had", ["have", "having", "has"], "easy",
         "have a great time 过去式", ["We had fun at the beach.（我们在海滩玩得开心。）"],
         "had a great time"),
        ("___ you watch the match last night?", "Did", ["Do", "Are", "Were"], "easy",
         "一般过去时疑问 Did", ["Did you go out yesterday?（你昨天出门了吗？）"],
         "Did + 原形"),
        ("She ___ me she was busy.", "told", ["tells", "telling", "has told"], "medium",
         "主句过去，从句也倾向过去", ["He said he was tired.（他说他很累。）"],
         "时态一致原则"),
        ("I used to ___ up late, but not anymore.", "stay", ["stayed", "staying", "stays"], "medium",
         "used to do 过去常常（现已不）", ["I used to play video games.（我以前常玩电子游戏。）"],
         "used to do 过去常常"),
        ("It ___ hotter than today.", "was", ["is", "has been", "were"], "easy",
         "过去的状态比较", ["Last summer was very hot.（去年夏天非常热。）"],
         "过去状态 was/were"),
        ("Who ___ the door?", "opened", ["open", "opening", "opens"], "easy",
         "过去发生的动作", ["Who broke the window?（谁打破了窗户？）"],
         "过去动作 过去式"),
        ("I ___ anything at the shop.", "didn't buy", ["don't buy", "haven't bought", "wasn't buying"], "easy",
         "didn't + 原形", ["She didn't come to school.（她没来上学。）"],
         "didn't + 原形"),
    ]

    for sent, ans, dist, d, exp, exs, mn in extra_simple_past:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_past", d, sent, "", o, ai, exp, exs, mn))

    # 补充现在完成时
    extra_pres_perf = [
        ("I ___ never ___ sushi before.", "have;tasted", ["did;taste", "do;taste", "am;tasting"], "medium",
         "never + 现在完成时", ["I have never been abroad.（我从未出过国。）"],
         "never → have/has never done"),
        ("She is the most talented person I ___.", "have ever met", ["ever met", "am meeting", "met"], "medium",
         "最高级 + have ever done", ["This is the best meal I have ever had.（这是我吃过最好的一餐。）"],
         "最高级 + have ever done"),
        ("___ you finished your work?", "Have", ["Do", "Did", "Are"], "easy",
         "现在完成时疑问 Have/Has", ["Has she called you?（她给你打电话了吗？）"],
         "Have/Has + 主语 + done"),
        ("We ___ each other for 10 years.", "have known", ["know", "knew", "are knowing"], "hard",
         "know 是状态动词，用 have known（不能用 have been knowing）",
         ["I have loved music since I was a child.（我从孩童时代起就热爱音乐。）"],
         "状态动词 have + 过去分词（非进行）"),
        ("I ___ my key. I can't get in.", "have lost", ["lost", "lose", "am losing"], "medium",
         "过去动作造成现在的结果", ["He has broken his leg. He can't walk.（他摔断了腿，不能走路了。）"],
         "结果影响现在 → 完成时"),
        ("There ___ many changes recently.", "have been", ["are", "were", "will be"], "medium",
         "recently 配合现在完成时", ["There have been several accidents lately.（最近发生了几起事故。）"],
         "recently → have/has been"),
    ]

    for sent, ans, dist, d, exp, exs, mn in extra_pres_perf:
        if ";" in ans:
            combined = ans.replace(";", " ")
            o, ai = shuffle_options(dist + [combined], 3)
            new_sent = sent.replace("___ ___", "___")
            qs.append(make_mc(nid(), "tense_present", d, new_sent, "", o, ai, exp, exs, mn))
        else:
            o, ai = shuffle_options(dist + [ans], 3)
            qs.append(make_mc(nid(), "tense_present", d, sent, "", o, ai, exp, exs, mn))

    # 补充一般将来时
    extra_future = [
        ("I promise I ___ be late again.", "won't", ["don't", "am not", "haven't"], "easy",
         "won't = will not 表承诺", ["I won't tell anyone.（我不会告诉任何人。）"],
         "won't 表承诺/拒绝"),
        ("___ God bless you.", "May", ["Will", "Shall", "Can"], "medium",
         "May + 主语 + 动词原形 表祝愿", ["May you succeed!（祝你成功！）"],
         "May 表祝愿"),
        ("The meeting ___ start soon.", "is to", ["does", "shall to", "will to"], "hard",
         "be to do 表按计划/规定", ["The president is to visit China next month.（总统将于下月访华。）"],
         "be to do = 计划/规定"),
        ("I ___ about 30 next birthday.", "will be", ["am", "am going to", "was"], "easy",
         "将来年龄/状态用 will be", ["She will be 16 next month.（下个月她就16岁了。）"],
         "将来年龄 will be"),
    ]

    for sent, ans, dist, d, exp, exs, mn in extra_future:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "tense_future", d, sent, "", o, ai, exp, exs, mn))

    # 大量补充 fill-in-the-blank 题目
    fill_extra = [
        # 一般现在时 fill
        ("He ___ (go) to school by bus every day.", "goes", "第三人称单数", "easy", "tense_present",
         "He <strong>goes</strong> to school by bus."),
        ("She usually ___ (get) up at 7.", "gets", "usually 提示一般现在时", "easy", "tense_present",
         "She usually <strong>gets</strong> up at 7."),
        ("The sun ___ (rise) in the east.", "rises", "客观真理", "easy", "tense_present",
         "The sun <strong>rises</strong> in the east."),
        ("___ (do) your parents work here?", "Do", "parents 复数用 Do", "easy", "tense_present",
         "<strong>Do</strong> your parents work here?"),
        ("Water ___ (freeze) at 0°C.", "freezes", "科学事实", "easy", "tense_present",
         "Water <strong>freeze</strong>s at 0°C."),
        ("He often ___ (watch) TV after dinner.", "watches", "often + 三单", "easy", "tense_present",
         "He often <strong>watches</strong> TV after dinner."),
        ("My dad ___ (read) newspapers every morning.", "reads", "read 的三单 reads", "easy", "tense_present",
         "My dad <strong>reads</strong> newspapers every morning."),
        ("They ___ (play) football on Sundays.", "play", "复数用原形", "easy", "tense_present",
         "They <strong>play</strong> football on Sundays."),
        ("___ (be) there a bank near here?", "Is", "bank 单数用 Is", "easy", "tense_present",
         "<strong>Is</strong> there a bank near here?"),
        ("She ___ (not like) spicy food.", "doesn't like", "否定 doesn't + 原形", "easy", "tense_present",
         "She <strong>doesn't like</strong> spicy food."),
        # 现在进行时 fill
        ("Look! It ___ (rain) now.", "is raining", "Look! 提示进行时", "easy", "tense_present",
         "Look! It <strong>is raining</strong> now."),
        ("I ___ (do) my homework at the moment.", "am doing", "at the moment", "easy", "tense_present",
         "I <strong>am doing</strong> my homework at the moment."),
        ("Listen! Someone ___ (sing).", "is singing", "Listen! 提示进行时", "easy", "tense_present",
         "Listen! Someone <strong>is singing</strong>."),
        ("We ___ (go) to Shanghai next week.", "are going", "计划好的将来", "medium", "tense_present",
         "We <strong>are going</strong> to Shanghai next week."),
        ("She ___ (write) a letter these days.", "is writing", "these days", "medium", "tense_present",
         "She <strong>is writing</strong> a letter these days."),
        ("They ___ (not watch) TV now.", "aren't watching", "否定进行时", "easy", "tense_present",
         "They <strong>aren't watching</strong> TV now."),
        # 现在完成时 fill
        ("I ___ (finish) my homework already.", "have finished", "already + 完成时", "medium", "tense_present",
         "I <strong>have finished</strong> my homework already."),
        ("She ___ (live) here since 2010.", "has lived", "since + 时间点", "medium", "tense_present",
         "She <strong>has lived</strong> here since 2010."),
        ("___ you ever ___ (see) a tiger?", "Have;seen", "ever + 完成时", "medium", "tense_present",
         "<strong>Have</strong> you ever <strong>seen</strong> a tiger?"),
        ("I ___ (not read) this book yet.", "haven't read", "yet + 否定完成时", "medium", "tense_present",
         "I <strong>haven't read</strong> this book yet."),
        ("He just ___ (arrive).", "has arrived", "just + 完成时", "medium", "tense_present",
         "He just <strong>has arrived</strong>."),
        ("We ___ (know) each other for 10 years.", "have known", "for + 时间段", "medium", "tense_present",
         "We <strong>have known</strong> each other for 10 years."),
        ("So far, we ___ (learn) 500 words.", "have learned", "so far", "medium", "tense_present",
         "So far, we <strong>have learned</strong> 500 words."),
        # 一般过去时 fill
        ("I ___ (go) to the park yesterday.", "went", "yesterday → 过去时", "easy", "tense_past",
         "I <strong>went</strong> to the park yesterday."),
        ("She ___ (come) back last night.", "came", "last night → 过去时", "easy", "tense_past",
         "She <strong>came</strong> back last night."),
        ("___ you ___ (watch) the movie?", "Did;watch", "过去时疑问 Did", "easy", "tense_past",
         "<strong>Did</strong> you <strong>watch</strong> the movie?"),
        ("I didn't ___ (eat) breakfast this morning.", "eat", "didn't + 原形", "easy", "tense_past",
         "I didn't <strong>eat</strong> breakfast this morning."),
        ("He ___ (be) born in 2005.", "was", "be born 过去时", "easy", "tense_past",
         "He <strong>was</strong> born in 2005."),
        ("When I was young, I ___ (like) candy.", "liked", "回忆过去", "easy", "tense_past",
         "When I was young, I <strong>liked</strong> candy."),
        ("We ___ (have) a great time at the party.", "had", "have 的过去式 had", "easy", "tense_past",
         "We <strong>had</strong> a great time at the party."),
        ("She ___ (tell) me a story.", "told", "tell 的过去式 told", "easy", "tense_past",
         "She <strong>told</strong> me a story."),
        # 过去进行时 fill
        ("I ___ (watch) TV when you called.", "was watching", "when 打断长动作", "medium", "tense_past",
         "I <strong>was watching</strong> TV when you called."),
        ("What ___ you ___ (do) at 8 last night?", "were;doing", "特定过去时刻", "medium", "tense_past",
         "What <strong>were</strong> you <strong>doing</strong> at 8 last night?"),
        ("While Mom ___ (cook), Dad read a newspaper.", "was cooking", "while 两个进行中", "medium", "tense_past",
         "While Mom <strong>was cooking</strong>, Dad read a newspaper."),
        ("They ___ (play) football from 3 to 5 yesterday.", "were playing", "过去时间段", "medium", "tense_past",
         "They <strong>were playing</strong> football from 3 to 5 yesterday."),
        # 过去完成时 fill
        ("When I arrived, the train ___ (leave) already.", "had left", "过去的过去", "hard", "tense_past",
         "When I arrived, the train <strong>had left</strong> already."),
        ("She said she ___ (see) the film before.", "had seen", "间接引语", "hard", "tense_past",
         "She said she <strong>had seen</strong> the film before."),
        ("By the end of last year, we ___ (learn) 2000 words.", "had learned", "by + 过去时间", "hard", "tense_past",
         "By the end of last year, we <strong>had learned</strong> 2000 words."),
        # 一般将来时 fill
        ("I think it ___ (rain) tomorrow.", "will rain", "主观判断", "medium", "tense_future",
         "I think it <strong>will rain</strong> tomorrow."),
        ("We ___ (visit) the Great Wall next week.", "are going to visit", "计划安排", "medium", "tense_future",
         "We <strong>are going to visit</strong> the Great Wall next week."),
        ("___ you please open the door?", "Will", "请求", "medium", "tense_future",
         "<strong>Will</strong> you please open the door?"),
        ("I ___ (not tell) anyone.", "won't tell", "承诺", "medium", "tense_future",
         "I <strong>won't tell</strong> anyone."),
        ("The train ___ (leave) in five minutes.", "is about to leave", "即将", "hard", "tense_future",
         "The train <strong>is about to leave</strong> in five minutes."),
        # 条件句 fill
        ("If it ___ (rain) tomorrow, we will stay home.", "rains", "主将从现", "medium", "tense_future",
         "If it <strong>rains</strong> tomorrow, we will stay home."),
        ("If I were you, I ___ (accept) it.", "would accept", "虚拟现在", "hard", "tense_future",
         "If I were you, I <strong>would accept</strong> it."),
        ("If I ___ (know) earlier, I would have told you.", "had known", "虚拟过去", "hard", "tense_future",
         "If I <strong>had known</strong> earlier, I would have told you."),
        ("I wish I ___ (be) rich now.", "were", "wish 虚拟现在", "hard", "tense_future",
         "I wish I <strong>were</strong> rich now."),
        ("I wish I ___ (study) harder then.", "had studied", "wish 虚拟过去", "hard", "tense_future",
         "I wish I <strong>had studied</strong> harder then."),
    ]

    for sent, ans, hint, d, cat, example in fill_extra:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans,
            f"根据上下文和关键词选择正确的时态形式。",
            [example],
            ""))

    return qs


# ============================================================
# 补充动词题 (确保达到 500+ 题)
# ============================================================

def gen_extra_verbs():
    """补充更多动词题"""
    qs = []

    # ===== 更多不规则动词 MC 题目 =====
    extra_irregular_mc = [
        ("What time did you ___ (get) up this morning?", "get", "did 后用原形", "easy",
         "What time did you <strong>get</strong> up?"),
        ("Have you ___ (eat) your lunch?", "eaten", "现在完成时用过去分词", "medium",
         "Have you <strong>eaten</strong> your lunch?"),
        ("She has ___ (write) three letters.", "written", "write 的过去分词 written", "medium",
         "She has <strong>written</strong> three letters."),
        ("They have ___ (go) home.", "gone", "go 的过去分词 gone", "easy",
         "They have <strong>gone</strong> home."),
        ("I have never ___ (ride) a horse.", "ridden", "ride 的过去分词 ridden", "hard",
         "I have never <strong>ridden</strong> a horse."),
        ("He has ___ (choose) the red one.", "chosen", "choose 的过去分词 chosen", "hard",
         "He has <strong>chosen</strong> the red one."),
        ("We have ___ (speak) to the teacher.", "spoken", "speak 的过去分词 spoken", "medium",
         "We have <strong>spoken</strong> to the teacher."),
        ("She has ___ (fly) to London twice.", "flown", "fly 的过去分词 flown", "hard",
         "She has <strong>flown</strong> to London twice."),
        ("The baby has ___ (fall) asleep.", "fallen", "fall 的过去分词 fallen", "medium",
         "The baby has <strong>fallen</strong> asleep."),
        ("Someone has ___ (steal) my bike!", "stolen", "steal 的过去分词 stolen", "hard",
         "Someone has <strong>stolen</strong> my bike!"),
        ("He has ___ (throw) the ball away.", "thrown", "throw 的过去分词 thrown", "hard",
         "He has <strong>thrown</strong> the ball away."),
        ("They have ___ (draw) a beautiful picture.", "drawn", "draw 的过去分词 drawn", "medium",
         "They have <strong>drawn</strong> a beautiful picture."),
        ("I have ___ (give) him the money.", "given", "give 的过去分词 given", "easy",
         "I have <strong>given</strong> him the money."),
        ("She has ___ (forget) my name.", "forgotten", "forget 的过去分词 forgotten", "hard",
         "She has <strong>forgotten</strong> my name."),
        ("We have ___ (swim) in the ocean.", "swum", "swim 的过去分词 swum", "hard",
         "We have <strong>swum</strong> in the ocean."),
        ("He has ___ (drive) that car before.", "driven", "drive 的过去分词 driven", "hard",
         "He has <strong>driven</strong> that car before."),
        ("The wind has ___ (blow) down the tree.", "blown", "blow 的过去分词 blown", "medium",
         "The wind has <strong>blown</strong> down the tree."),
        ("She has ___ (wear) that dress before.", "worn", "wear 的过去分词 worn", "medium",
         "She has <strong>worn</strong> that dress before."),
        ("Who has ___ (break) the window?", "broken", "break 的过去分词 broken", "easy",
         "Who has <strong>broken</strong> the window?"),
        ("The flowers have ___ (grow) tall.", "grown", "grow 的过去分词 grown", "medium",
         "The flowers have <strong>grown</strong> tall."),
        ("I have ___ (know) him for years.", "known", "know 的过去分词 known", "easy",
         "I have <strong>known</strong> him for years."),
        ("Has she ___ (catch) the bus?", "caught", "catch 的过去分词 caught", "medium",
         "Has she <strong>caught</strong> the bus?"),
        ("They have ___ (teach) us a lot.", "taught", "teach 的过去分词 taught", "medium",
         "They have <strong>taught</strong> us a lot."),
        ("He has ___ (sell) his old car.", "sold", "sell 的过去分词 sold", "easy",
         "He has <strong>sold</strong> his old car."),
        ("We have ___ (understand) the lesson.", "understood", "understand 的过去分词 understood", "hard",
         "We have <strong>understood</strong> the lesson."),
        ("The plane has ___ (rise) into the sky.", "risen", "rise 的过去分词 risen", "hard",
         "The plane has <strong>risen</strong> into the sky."),
        ("She has ___ (wake) everyone up.", "woken", "wake 的过去分词 woken", "hard",
         "She has <strong>woken</strong> everyone up."),
        ("It has ___ (become) very cold.", "become", "become 的过去分词 become（不变）", "medium",
         "It has <strong>become</strong> very cold."),
        ("He has ___ (fight) bravely.", "fought", "fight 的过去分词 fought", "hard",
         "He has <strong>fought</strong> bravely."),
        ("I have ___ (hide) the gift.", "hidden", "hide 的过去分词 hidden", "hard",
         "I have <strong>hidden</strong> the gift."),
        ("The price has ___ (fall) by 10%.", "fallen", "fall 的过去分词 fallen", "medium",
         "The price has <strong>fallen</strong> by 10%."),
        ("She has ___ (keep) the secret.", "kept", "keep 的过去分词 kept", "easy",
         "She has <strong>kept</strong> the secret."),
        ("We have ___ (meet) before.", "met", "meet 的过去分词 met", "easy",
         "We have <strong>met</strong> before."),
        ("He has ___ (send) the email.", "sent", "send 的过去分词 sent", "easy",
         "He has <strong>sent</strong> the email."),
        ("They have ___ (build) a new house.", "built", "build 的过去分词 built", "medium",
         "They have <strong>built</strong> a new house."),
        ("I have ___ (lend) him my book.", "lent", "lend 的过去分词 lent", "medium",
         "I have <strong>lent</strong> him my book."),
        ("She has ___ (lose) her keys again!", "lost", "lose 的过去分词 lost", "easy",
         "She has <strong>lost</strong> her keys again!"),
        ("The bell has ___ (ring).", "rung", "ring 的过去分词 rung", "hard",
         "The bell has <strong>rung</strong>."),
        ("He has ___ (shake) hands with everyone.", "shaken", "shake 的过去分词 shaken", "hard",
         "He has <strong>shaken</strong> hands with everyone."),
        ("She has ___ (show) me the photos.", "shown", "show 的过去分词 shown", "medium",
         "She has <strong>shown</strong> me the photos."),
        ("The baby has ___ (shut) the door.", "shut", "shut 的过去分词 shut（不变）", "hard",
         "The baby has <strong>shut</strong> the door."),
        ("We have ___ (sweep) the floor.", "swept", "sweep 的过去分词 swept", "hard",
         "We have <strong>swept</strong> the floor."),
        ("He has ___ (sing) two songs.", "sung", "sing 的过去分词 sung", "hard",
         "He has <strong>sung</strong> two songs."),
        ("I have ___ (pay) for the ticket.", "paid", "pay 的过去分词 paid", "easy",
         "I have <strong>paid</strong> for the ticket."),
        ("She has ___ (set) the table.", "set", "set 的过去分词 set（不变）", "hard",
         "She has <strong>set</strong> the table."),
        ("The sun has ___ (shine) all day.", "shone", "shine 的过去分词 shone", "hard",
         "The sun has <strong>shone</strong> all day."),
        ("He has ___ (win) first prize.", "won", "win 的过去分词 won", "easy",
         "He has <strong>won</strong> first prize."),
        ("I have ___ (mean) what I said.", "meant", "mean 的过去分词 meant", "hard",
         "I have <strong>meant</strong> what I said."),
        ("She has ___ (feel) better.", "felt", "feel 的过去分词 felt", "easy",
         "She has <strong>felt</strong> better."),
        ("We have ___ (leave) school early.", "left", "leave 的过去分词 left", "easy",
         "We have <strong>left</strong> school early."),
        ("He has ___ (think) about it.", "thought", "think 的过去分词 thought", "easy",
         "He has <strong>thought</strong> about it."),
        ("She has ___ (buy) a new phone.", "bought", "buy 的过去分词 bought", "easy",
         "She has <strong>bought</strong> a new phone."),
        ("They have ___ (bring) some snacks.", "brought", "bring 的过去分词 brought", "easy",
         "They have <strong>brought</strong> some snacks."),
        ("I have ___ (cut) my finger.", "cut", "cut 的过去分词 cut（不变）", "hard",
         "I have <strong>cut</strong> my finger."),
        ("He has ___ (hurt) his leg.", "hurt", "hurt 的过去分词 hurt（不变）", "hard",
         "He has <strong>hurt</strong> his leg."),
        ("The cost has ___ (cost) too much.", "cost", "cost 的过去分词 cost（不变）", "hard",
         "The cost has <strong>cost</strong> too much."),
        ("I have ___ (hit) the ball.", "hit", "hit 的过去分词 hit（不变）", "hard",
         "I have <strong>hit</strong> the ball."),
        ("She has ___ (let) the cat out.", "let", "let 的过去分词 let（不变）", "hard",
         "She has <strong>let</strong> the cat out."),
        ("We have ___ (put) everything away.", "put", "put 的过去分词 put（不变）", "hard",
         "We have <strong>put</strong> everything away."),
        ("He has ___ (read) this book twice.", "read", "read 的过去分词 read（拼写不变）", "hard",
         "He has <strong>read</strong> this book twice."),
        ("It has ___ (lead) to success.", "led", "lead 的过去分词 led", "medium",
         "It has <strong>led</strong> to success."),
        ("She has ___ (learn) French.", "learnt/learned", "learn 的过去分词 learnt/learned", "medium",
         "She has <strong>learnt/learned</strong> French."),
        ("He has ___ (smell) something burning.", "smelt/smelled", "smell 的过去分词 smelt/smelled", "hard",
         "He has <strong>smelt/smelled</strong> something burning."),
        ("I have ___ (stand) here for an hour.", "stood", "stand 的过去分词 stood", "easy",
         "I have <strong>stood</strong> here for an hour."),
        ("She has ___ (sit) there all morning.", "sat", "sit 的过去分词 sat", "easy",
         "She has <strong>sat</strong> there all morning."),
        ("The baby has ___ (sleep) for hours.", "slept", "sleep 的过去分词 slept", "easy",
         "The baby has <strong>slept</strong> for hours."),
        ("He has ___ (run) 10 kilometers.", "run", "run 的过去分词 run（不变）", "hard",
         "He has <strong>run</strong> 10 kilometers."),
        ("I have ___ (begin) to understand.", "begun", "begin 的过去分词 begun", "hard",
         "I have <strong>begun</strong> to understand."),
        ("She has ___ (drink) all the juice.", "drunk", "drink 的过去分词 drunk", "hard",
         "She has <strong>drunk</strong> all the juice."),
        ("We have ___ (fall) in love.", "fallen", "fall 的过去分词 fallen", "medium",
         "We have <strong>fallen</strong> love."),
        ("He has ___ (lie) in bed all day.", "lain", "lie(躺)的过去分词 lain", "hard",
         "He has <strong>lain</strong> in bed all day."),
        ("I have ___ (make) a decision.", "made", "make 的过去分词 made", "easy",
         "I have <strong>made</strong> a decision."),
        ("She has ___ (do) her best.", "done", "do 的过去分词 done", "easy",
         "She has <strong>done</strong> her best."),
        ("They have ___ (say) nothing.", "said", "say 的过去分词 said", "easy",
         "They have <strong>said</strong> nothing."),
        ("He has ___ (tell) the truth.", "told", "tell 的过去分词 told", "easy",
         "He has <strong>told</strong> the truth."),
        ("We have ___ (see) that movie.", "seen", "see 的过去分词 seen", "easy",
         "We have <strong>seen</strong> that movie."),
        ("She has ___ (get) a new job.", "got/gotten", "get 的过去分词 got/gotten", "easy",
         "She has <strong>got/gotten</strong> a new job."),
        ("I have ___ (give) it to him.", "given", "give 的过去分词 given", "easy",
         "I have <strong>given</strong> it to him."),
        ("He has ___ (be) very busy.", "been", "be 的过去分词 been", "easy",
         "He has <strong>been</strong> very busy."),
        ("They have ___ (have) dinner.", "had", "have 的过去分词 had", "easy",
         "They have <strong>had</strong> dinner."),
        ("I have ___ (hear) the news.", "heard", "hear 的过去分词 heard", "easy",
         "I have <strong>heard</strong> the news."),
        ("She has ___ (hold) the record.", "held", "hold 的过去分词 held", "medium",
         "She has <strong>held</strong> the record."),
        ("We have ___ (feel) happy.", "felt", "feel 的过去分词 felt", "easy",
         "We have <strong>felt</strong> happy."),
        ("He has ___ (leave) already.", "left", "leave 的过去分词 left", "easy",
         "He has <strong>left</strong> already."),
        ("I have ___ (lose) my way.", "lost", "lose 的过去分词 lost", "easy",
         "I have <strong>lost</strong> my way."),
        ("She has ___ (meet) him before.", "met", "meet 的过去分词 met", "easy",
         "She has <strong>met</strong> him before."),
        ("They have ___ (pay) the bill.", "paid", "pay 的过去分词 paid", "easy",
         "They have <strong>paid</strong> the bill."),
        ("He has ___ (put) on weight.", "put", "put 的过去分词 put（不变）", "hard",
         "He has <strong>put</strong> on weight."),
        ("I have ___ (read) the letter.", "read", "read 的过去分词 read（拼写不变）", "hard",
         "I have <strong>read</strong> the letter."),
        ("She has ___ (ride) a bike.", "ridden", "ride 的过去分词 ridden", "hard",
         "She has <strong>ridden</strong> a bike."),
        ("We have ___ (ring) the doorbell.", "rung", "ring 的过去分词 rung", "hard",
         "We have <strong>rung</strong> the doorbell."),
        ("He has ___ (rise) early today.", "risen", "rise 的过去分词 risen", "hard",
         "He has <strong>risen</strong> early today."),
        ("I have ___ (run) out of time.", "run", "run 的过去分词 run（不变）", "hard",
         "I have <strong>run</strong> out of time."),
        ("She has ___ (sing) beautifully.", "sung", "sing 的过去分词 sung", "hard",
         "She has <strong>sung</strong> beautifully."),
        ("They have ___ (sit) together.", "sat", "sit 的过去分词 sat", "easy",
         "They have <strong>sat</strong> together."),
        ("I have ___ (sleep) well.", "slept", "sleep 的过去分词 slept", "easy",
         "I have <strong>slept</strong> well."),
        ("He has ___ (smell) the flowers.", "smelt/smelled", "smell 的过去分词 smelt/smelled", "hard",
         "He has <strong>smelt/smelled</strong> the flowers."),
        ("We have ___ (speak) English.", "spoken", "speak 的过去分词 spoken", "medium",
         "We have <strong>spoken</strong> English."),
        ("She has ___ (stand) by me.", "stood", "stand 的过去分词 stood", "easy",
         "She has <strong>stood</strong> by me."),
        ("I have ___ (steal) nothing.", "stolen", "steal 的过去分词 stolen", "hard",
         "I have <strong>stolen</strong> nothing."),
        ("They have ___ (sweep) the room.", "swept", "sweep 的过去分词 swept", "hard",
         "They have <strong>swept</strong> the room."),
        ("He has ___ (swim) across the lake.", "swum", "swim 的过去分词 swum", "hard",
         "He has <strong>swum</strong> across the lake."),
        ("I have ___ (take) the test.", "taken", "take 的过去分词 taken", "easy",
         "I have <strong>taken</strong> the test."),
        ("She has ___ (teach) for 10 years.", "taught", "teach 的过去分词 taught", "medium",
         "She has <strong>taught</strong> for 10 years."),
        ("We have ___ (tell) stories.", "told", "tell 的过去分词 told", "easy",
         "We have <strong>told</strong> stories."),
        ("He has ___ (throw) away the trash.", "thrown", "throw 的过去分词 thrown", "hard",
         "He has <strong>thrown</strong> away the trash."),
        ("I have ___ (wake) up early.", "woken/waked", "wake 的过去分词 woken/waked", "hard",
         "I have <strong>woken/waked</strong> up early."),
        ("She has ___ (wear) that coat.", "worn", "wear 的过去分词 worn", "medium",
         "She has <strong>worn</strong> that coat."),
        ("They have ___ (win) every game.", "won", "win 的过去分词 won", "easy",
         "They have <strong>won</strong> every game."),
        ("I have ___ (write) a poem.", "written", "write 的过去分词 written", "medium",
         "I have <strong>written</strong> a poem."),
    ]

    for sent, ans, hint, d, example in extra_irregular_mc:
        qs.append(make_fill(nid(), "verb", d, sent, hint, ans,
            f"根据时态要求填写正确的动词形式。",
            [example],
            ""))

    # ===== 更多动词辨析 MC =====
    more_pairs = [
        # accept vs receive
        ("I ___ his invitation happily.", "accepted", ["received", "got", "took"],
         "accept 表示\"接受（同意）\"；receive 表示\"收到（未必接受）\"。",
         ["I received his letter but didn't accept his offer.（我收到了他的信但没接受他的提议。）"],
         "接受 accept; 收到 receive"),
        # join / take part in / attend
        ("He ___ the army last year.", "joined", ["joined in", "took part in", "attended"],
         "join 加入组织/团体（army, club, party）；take part in 参加活动。",
         ["He joined the Party last year.（他去年入党了。）",
          "She joined the music club.（她加入了音乐俱乐部。）"],
         "加入组织 join"),
        ("She ___ the meeting yesterday.", "attended", ["joined", "joined in", "took part in"],
         "attend 出席（会议、婚礼、典礼等正式场合）。",
         ["Did you attend the lecture?（你去听讲座了吗？）",
          "She attended the wedding.（她参加了婚礼。）"],
         "出席 attend"),
        # beat / win
        ("Our team ___ theirs 3-1.", "beat", ["won", "defeated", "gained"],
         "beat + 对手（人/队）；win + 比赛/奖品。",
         ["We beat Class 2 in basketball.（我们在篮球赛中打败了2班。）"],
         "打败对手 beat"),
        ("She ___ first prize.", "won", ["beat", "got", "gained"],
         "win + 奖品/比赛名次。",
         ["He won the gold medal.（他赢得了金牌。）"],
         "赢得奖品 win"),
        # borrow / keep
        ("How long can I ___ this book?", "keep", ["borrow", "lend", "rent"],
         "borrow 是瞬间动作不能与 how long 连用；keep 表示\"保留、借（持续）\"。",
         ["You can keep it for a week.（你可以借一周。）"],
         "延续性借用 keep"),
        # die / dead / death / dying
        ("His grandfather ___ two years ago.", "died", ["was dead", "has died", "has been dead"],
         "two years ago 用一般过去时 died；be dead 可以和时间段连用。",
         ["He has been dead for two years.（他已经去世两年了。——强调状态持续）"],
         "died 过去动作; been dead 持续状态"),
        # cost / spend / pay / take 续
        ("It ___ me two hours to finish.", "took", ["spent", "cost", "paid"],
         "It takes sb. time to do sth.",
         ["It took us three days.（花了我们三天时间。）"],
         "It takes time to do"),
        ("She ___ 100 yuan on the dress.", "spent", ["cost", "paid", "took"],
         "sb. spends money on sth.",
         ["I spent 50 yuan on this book.（这本书花了我50元。）"],
         "spend money on sth"),
        # fit / suit / match
        ("This coat ___ me well.", "fits", ["suits", "matches", "likes"],
         "fit 表示尺寸合适；suit 表示款式/颜色适合；match 表示搭配协调。",
         ["These shoes don't fit me.（这鞋不合脚。）",
          "Blue suits you.（蓝色很适合你。）"],
         "尺寸合身 fit; 样式适合 suit"),
        # hurt / wound / injure
        ("He ___ his leg playing football.", "injured", ["hurt", "wounded", "harmed"],
         "injure 指意外伤害（事故等）；hurt 可指身体或情感伤害；wound 指刀枪伤。",
         ["He was injured in a car accident.（他在车祸中受伤了。）",
          "My head hurts.（我头疼。）"],
         "事故伤害 injure; 刀枪伤 wound"),
        # pull / push
        ("Please ___ the door open. It's stuck.", "push", ["pull", "press", "drag"],
         "push 推；pull 拉。",
         ["Pull the door, don't push it.（拉门，别推。）"],
         "推 push; 拉 pull"),
        # lay (放) / lie (躺)
        ("The hen ___ an egg every day.", "lays", ["lies", "laid", "lies down"],
         "lay(下蛋/放置)的第三人称单数是 lays。",
         ["A hen lays eggs.（母鸡下蛋。）"],
         "下蛋 lays (lay的单三)"),
        ("Don't ___ on the bed with shoes!", "lie", ["lay", "lain", "laid"],
         "lie(躺)的原形命令句。",
         ["Lie down and rest.（躺下休息吧。）"],
         "躺 lie (原形)"),
        # sit / seat
        ("Please be ___. The meeting will begin.", "seated", ["sat", "sitting", "seat"],
         "seat 是及物动词，be seated = 请坐（正式）。",
         ["Please be seated.（请就座。）"],
         "就座 be seated (正式)"),
        # rise / raise / arise / arouse
        ("The price has ___ by 10%.", "risen", ["raised", "arose", "aroused"],
         "price 自然上涨用 rise（不及物）。",
         ["Prices have risen sharply.（价格急剧上涨。）"],
         "价格上涨 rises"),
        ("Can you ___ your hand?", "raise", ["rise", "lift", "arise"],
         "raise 举手（及物动词）。",
         ["Raise your hand if you know.（知道的请举手。）"],
         "举手 raise"),
        # hang
        ("She ___ the picture on the wall.", "hung", ["hanged", "hang", "hanging"],
         "hang（悬挂）的过去式是 hung；hang（绞死）的过去式是 hanged。",
         ["Pictures are hung on the wall.（画挂在墙上。）"],
         "悬挂 hung/hung; 绞死 hanged/hanged"),
        # learn / study
        ("I want to ___ driving.", "learn", ["study", "know", "understand"],
         "learn 多指学习技能/初级阶段；study 多指系统学习/研究。",
         ["I'm learning to swim.（我在学游泳。）",
          "He studies at Beijing University.（他在北京大学就读。）"],
         "学技能 learn; 系统学 study"),
        # speak / say / tell / talk 续
        ("He can ___ three languages.", "speak", ["say", "tell", "talk"],
         "speak + 语言名称。",
         ["Do you speak Chinese?（你会说中文吗？）"],
         "说语言 speak"),
        ("Don't forget to ___ \"thank you\".", "say", ["speak", "tell", "talk"],
         "say + 直接引语（说的话的内容）。",
         ["Say \"hello\" to him for me.（替我向他问好。）"],
         "说内容 say"),
        ("Can you ___ me the time?", "tell", ["say", "speak", "talk"],
         "tell sb. sth.（告诉某人某事）。",
         ["Can you tell me the way to the station?（你能告诉我去车站的路吗？）"],
         "告诉 tell sb. sth"),
        ("Stop ___ in class!", "talking", ["saying", "speaking", "telling"],
         "talk 表示交谈互动（不及物，常含贬义指闲聊）。",
         ["No talking during the exam!（考试期间禁止交谈！）"],
         "闲聊 talking"),
        # see / watch / look / notice / observe
        ("Did you ___ the news?", "watch", ["see", "look", "notice"],
         "看新闻/电视节目用 watch。",
         ["I watched a movie last night.（昨晚我看了一部电影。）"],
         "看电视新闻 watch"),
        ("I ___ something strange.", "noticed", ["saw", "watched", "observed"],
         "notice 注意到（有意识或无意识地察觉）。",
         "I noticed he looked tired.（我注意到他看起来很累了。）",
         "注意到 notice"),
        # hear / listen to / sound
        ("That ___ like a good idea.", "sounds", ["hears", "listens", "listens to"],
         "sound + 形容词表示\"听起来...\"（系动词用法）。",
         ["Sounds great!（听起来不错！）",
          "That sounds interesting.（那听起来很有趣。）"],
         "听起来 sound (+ adj)"),
        # bring / take / carry / fetch
        ("Can you ___ this bag for me?", "carry", ["bring", "take", "fetch"],
         "carry 携带（无方向性）；bring 带来；take 带走；fetch 去拿来。",
         ["She carried a heavy box.（她搬着一个沉重的箱子。）"],
         "携带 carry (无方向)"),
        ("Go and ___ some water.", "fetch", ["bring", "take", "carry"],
         "fetch 去取来（往返动作）。",
         ["Fetch me some water, please.（请帮我拿点水来。）"],
         "去取来 fetch (往返)"),
        # refuse / reject / decline
        ("He ___ my invitation politely.", "declined", ["refused", "rejected", "denied"],
         "decline 婉拒（客气地拒绝）；refuse 直接拒绝；reject 正式驳回。",
         ["She declined the offer.（她婉拒了这个提议。）",
          "He refused to help.（他拒绝帮忙。）"],
         "婉拒 decline; 拒绝 refuse; 驳回 reject"),
    ]

    for sent, ans, dist, exp, exs, mn in more_pairs:
        o, ai = shuffle_options(dist + [ans], 3)
        d = "medium"
        qs.append(make_mc(nid(), "verb", d, sent, "", o, ai, exp, exs, mn))

    # ===== 更多 make/do 搭配 =====
    more_make_do = [
        ("Let's ___ progress together.", "make", ["do", "take", "get"], "medium",
         "make progress 取得进步", "make progress"),
        ("___ room for the car.", "Make", ["Do", "Take", "Get"], "easy",
         "make room 腾地方", "make room"),
        ("Don't ___ faces at me!", "make", ["do", "take", "give"], "easy",
         "make faces 做鬼脸", "make faces"),
        ("She always ___ excuses.", "makes", ["does", "takes", "gets"], "medium",
         "make excuses 找借口", "make excuses"),
        ("Can you ___ me a favor?", "do", ["make", "take", "give"], "easy",
         "do sb. a favor 帮忙", "do sb. a favor"),
        ("Just ___ your best!", "do", ["make", "take", "try"], "easy",
         "do one's best 尽力", "do one's best"),
        ("I need to ___ the dishes.", "do", ["make", "take", "wash"], "easy",
         "do the dishes 洗碗/餐具", "do the dishes"),
        ("She ___ business with many countries.", "does", ["makes", "takes", "gets"], "medium",
         "do做生意 经商", "do business"),
        ("Let's ___ a deal.", "make", ["do", "take", "have"], "medium", 
         "make a deal 达成协议", "make a deal"),
        ("He ___ a living by teaching.", "makes", ["does", "takes", "earns"], "medium", 
         "make a谋生 生计", "make a living"),
        ("Don't ___ trouble!", "make", ["do", "take", "give"], "easy",
         "make trouble 制造麻烦", "make trouble"),
        ("Can I ___ a suggestion?", "make", ["do", "take", "give"], "medium", 
         "make a suggestion 提建议", "make a suggestion"),
        ("She ___ friends easily.", "makes", ["does", "gets", "takes"], "easy",
         "make friends 交朋友", "make friends"),
        ("They ___ fun of him.", "made", ["did", "took", "got"], "medium",
         "make fun of 取笑", "make fun of"),
        ("I'll ___ sure of that.", "make", ["do", "take", "get"], "easy",
         "make sure 确保", "make sure"),
        ("He ___ his bed every morning.", "makes", ["does", "takes", "cleans"], "easy",
         "make the bed 整理床铺", "make the bed"),
    ]

    for sent, ans, dist, d, key, _ in more_make_do:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", d, sent, "", o, ai,
            f"{key} 是固定搭配。", [f"{sent.replace('___', f'<strong>{ans}</strong>')}"], key))

    # ===== 更多情态动词 =====
    more_modals = [
        ("You ___ be kidding!", "must", ["can", "may", "will"], "medium",
         "must be 一定是对把握很大的推测", "must be 一定是"),
        ("That ___ be true—it's impossible!", "can't", ["mustn't", "might not", "shouldn't"], "medium",
         "can't be 不可能是", "can't be 不可能"),
        ("___ I come in?", "May", ["Must", "Can", "Do"], "easy",
         "May I 正式请求许可", "May I ...?"),
        ("You ___ smoke here. It's dangerous!", "mustn't", ["needn't", "don't", "won't"], "easy",
         "mustn't 禁止", "mustn't 禁止"),
        ("___ we go swimming?", "Shall", ["Will", "Would", "Should"], "easy",
         "Shall we 提议", "Shall we ...?"),
        ("I ___ rather stay home.", "'d", ["will", "am", "shall"], "medium",
         "would rather 宁愿", "would rather"),
        ("You ___ hurry or you'll be late.", "had better", ["had rather", "would better", "should better"], "hard",
         "had better 最好", "had better"),
        ("___ God bless you.", "May", ["Must", "Can", "Will"], "medium",
         "May 祝愿", "May God ..."),
        ("You ___ worry about me.", "needn't", ["mustn't", "can't", "won't"], "medium",
         "needn't 不必", "needn't 不必"),
        ("Students ___ obey school rules.", "ought to", ["must", "should", "can"], "medium",
         "ought to = should 应该", "ought to = should"),
        ("How ___ you say such a thing?", "dare", ["need", "must", "can"], "hard",
         "dare 敢（贬义）", "How dare you"),
        ("You ___ use my computer anytime.", "can", ["must", "shall", "will"], "easy",
         "can 许可", "can 许可"),
        ("___ I help you?", "Can/May", ["Will", "Do", "Shall"], "easy",
         "Can/May I help 你", "Can I help you"),
        ("She ___ play piano when she was five.", "could", ["can", "may", "might"], "easy",
         "could 过去的能力", "could 能（过去）"),
        ("It ___ rain. Look at those clouds.", "might", ["must", "can", "will"], "medium",
         "might 可能性较小", "might 可能"),
    ]

    for sent, ans, dist, d, _, mn in more_modals:
        o, ai = shuffle_options(dist + [ans], 3)
        qs.append(make_mc(nid(), "verb", d, sent, "", o, ai,
            f"情态动词 {ans} 的用法", [sent.replace('___', f'<strong>{ans}</strong>')], mn))

    # ===== 更多 doing vs to do =====
    more_gerund_inf = [
        ("I enjoy ___ (read) novels.", "reading", "enjoy doing", "easy",
         "I enjoy <strong>reading</strong> novels."),
        ("She finished ___ (clean) the room.", "cleaning", "finish doing", "easy",
         "She finished <strong>cleaning</strong> the room."),
        ("Practice ___ (speak) English every day.", "speaking", "practice doing", "easy",
         "Practice <strong>speaking</strong> English every day."),
        ("Would you mind ___ (open) the window?", "opening", "mind doing", "medium",
         "Would you mind <strong>opening</strong> the window?"),
        ("You can't avoid ___ (make) mistakes.", "making", "avoid doing", "medium",
         "You can't avoid <strong>making</strong> mistakes."),
        ("I suggest ___ (go) by taxi.", "going", "suggest doing", "medium",
         "I suggest <strong>going</strong> by taxi."),
        ("I consider ___ (study) abroad.", "studying", "consider doing", "medium",
         "I consider <strong>studying</strong> abroad."),
        ("Keep ___ (try)! Don't give up.", "trying", "keep doing", "easy",
         "Keep <strong>trying</strong>! Don't give up."),
        ("Give up ___ (smoke). It's bad for health.", "smoking", "give up doing", "medium",
         "Give up <strong>smoking</strong>. It's bad for health."),
        ("I can't help ___ (laugh).", "laughing", "can't help doing", "medium",
         "I can't help <strong>laughing</strong>."),
        ("I feel like ___ (have) a rest.", "having", "feel like doing", "medium",
         "I feel like <strong>having</strong> a rest."),
        ("I look forward to ___ (see) you.", "seeing", "look forward to doing", "hard",
         "I look forward to <strong>seeing</strong> you."),
        ("I'm used to ___ (get) up early.", "getting", "be used to doing", "hard",
         "I'm used to <strong>getting</strong> up early."),
        ("She spent two hours ___ (do) homework.", "doing", "spend time doing", "medium",
         "She spent two hours <strong>doing</strong> homework."),
        ("He is busy ___ (prepare) for the exam.", "preparing", "busy doing", "medium",
         "He is busy <strong>preparing</strong> for the exam."),
        ("We had fun ___ (play) games.", "playing", "have fun doing", "easy",
         "We had fun <strong>playing</strong> games."),
        ("I want ___ (learn) Japanese.", "to learn", "want to do", "easy",
         "I want <strong>to learn</strong> Japanese."),
        ("She hopes ___ (visit) Japan.", "to visit", "hope to do", "easy",
         "She hopes <strong>to visit</strong> Japan."),
        ("They decided ___ (start) a company.", "to start", "decide to do", "easy",
         "They decided <strong>to start</strong> a company."),
        ("We plan ___ (travel) this summer.", "to travel", "plan to do", "easy",
         "We plan <strong>to travel</strong> this summer."),
        ("He promised ___ (help) me.", "to help", "promise to do", "medium",
         "He promised <strong>to help</strong> me."),
        ("She offered ___ (drive) me home.", "to drive", "offer to do", "medium",
         "She offered <strong>to drive</strong> me home."),
        ("He refused ___ (answer).", "to answer", "refuse to do", "medium",
         "He refused <strong>to answer</strong>."),
        ("They agreed ___ (meet) at 7.", "to meet", "agree to do", "medium",
         "They agreed <strong>to meet</strong> at 7."),
        ("Would you like ___ (join) us?", "to join", "would like to do", "easy",
         "Would you like <strong>to join</strong> us?"),
        ("Mom asked me ___ (clean) my room.", "to clean", "ask sb. to do", "medium",
         "Mom asked me <strong>to clean</strong> my room."),
        ("The teacher told us ___ (be quiet).", "to be", "tell sb. to do", "medium",
         "The teacher told us <strong>to be</strong> quiet."),
        ("It took me an hour ___ (finish).", "to finish", "It takes to do", "medium",
         "It took me an hour <strong>to finish</strong>."),
        ("He is too young ___ (drive).", "to drive", "too...to...", "medium",
         "He is too young <strong>to drive</strong>."),
        ("She is old enough ___ (vote).", "to vote", "enough...to...", "medium",
         "She is old enough <strong>to vote</strong>."),
        ("Remember ___ (lock) the door.", "to lock", "remember to do (未做)", "hard",
         "Remember <strong>to lock</strong> the door.（记得去锁——还没锁）"),
        ("I remember ___ (lock) the door.", "locking", "remember doing (已做)", "hard",
         "I remember <strong>locking</strong> the door.（我记得锁了门）"),
        ("I forgot ___ (call) her.", "to call", "forget to do (没做)", "hard",
         "I forgot <strong>to call</strong> her.（忘了打——没打）"),
        ("I'll never forget ___ (visit) Beijing.", "visiting", "forget doing (做过)", "hard",
         "I'll never forget <strong>visiting</strong> Beijing.（永远不会忘记去过北京）"),
        ("He stopped ___ (have) a rest.", "to have", "stop to do (停下来去做)", "hard",
         "He stopped <strong>to have</strong> a rest.（停下休息）"),
        ("Stop ___ (talk)! The teacher is coming.", "talking", "stop doing (停止做)", "hard",
         "Stop <strong>talking</strong>!（停止说话）"),
        ("Try ___ (solve) it another way.", "to solve", "try to do (尽力做)", "hard",
         "Try <strong>to solve</strong> it another way.（尽力解决）"),
        ("Try ___(add) more salt.", "adding", "try doing (尝试做)", "hard",
         "Try <strong>adding</strong> more salt.（试试多加盐）"),
        ("After finishing math, she went on ___ (read) English.", "to read", "go on to do (换事)", "hard",
         "She went on <strong>to read</strong> English.（接着读英语）"),
        ("Go on ___ (read), please.", "reading", "go on doing (继续)", "hard",
         "Go on <strong>reading</strong>, please.（继续读）"),
        ("Missing the train means ___ (wait) for an hour.", "waiting", "mean doing (意味着)", "hard",
         "Missing the train means <strong>waiting</strong> for an hour.（意味着要等一小时）"),
        ("I mean ___ (help) you.", "to help", "mean to do (打算)", "hard",
         "I mean <strong>to help</strong> you.（我打算帮你）"),
    ]

    for sent, ans, hint, d, example in more_gerund_inf:
        qs.append(make_fill(nid(), "verb", d, sent, hint, ans,
            f"根据动词后面接 doing 还是 to do 的规则选择正确形式。",
            [example],
            ""))

    return qs


def gen_extra_tenses():
    """补充时态题到 500+ """
    qs = []

    # 大量一般现在时补充
    sp_more = [
        ("My brother ___ (study) in No.1 Middle School.", "studies", "三单 studies", "easy", "tense_present",
         "My brother <strong>studies</strong> in No.1 Middle School."),
        ("The moon ___ (move) around the earth.", "moves", "客观真理", "easy", "tense_present",
         "The moon <strong>moves</strong> around the earth."),
        ("___ (do) she like apples?", "Does", "三单 Does", "easy", "tense_present",
         "<strong>Does</strong> she like apples?"),
        ("There ___ (be) many books on the shelf.", "are", "复数 are", "easy", "tense_present",
         "There <strong>are</strong> many books on the shelf."),
        ("He ___ (not have) any money.", "doesn't have", "三单否定", "easy", "tense_present",
         "He <strong>doesn't have</strong> any money."),
        ("Where ___ (do) your parents live?", "do", "复数 Do", "easy", "tense_present",
         "Where <strong>do</strong> your parents live?"),
        ("She always ___ (wear) a uniform to school.", "wears", "三单 wears", "easy", "tense_present",
         "She always <strong>wears</strong> a uniform to school."),
        ("The train ___ (leave) at 9 AM sharp.", "leaves", "时刻表将来", "medium", "tense_present",
         "The train <strong>leaves</strong> at 9 AM sharp."),
        ("Light ___ (travel) faster than sound.", "travels", "科学事实", "easy", "tense_present",
         "Light <strong>travels</strong> faster than sound."),
        ("___ (be) your mother a teacher?", "Is", "三单 Is", "easy", "tense_present",
         "<strong>Is</strong> your mother a teacher?"),
        ("Tom ___ (not watch) TV on weekdays.", "doesn't watch", "否定", "easy", "tense_present",
         "Tom <strong>doesn't watch</strong> TV on weekdays."),
        ("My dad ___ (work) in a bank.", "works", "三单 works", "easy", "tense_present",
         "My dad <strong>works</strong> in a bank."),
        ("Birds ___ (fly) south in winter.", "fly", "复数原形", "easy", "tense_present",
         "Birds <strong>fly</strong> south in winter."),
        ("___ (be) they at home now?", "Are", "复数 Are", "easy", "tense_present",
         "<strong>Are</strong> they at home now?"),
        ("She never ___ (drink) coffee.", "drinks", "三单 drinks", "easy", "tense_present",
         "She never <strong>drinks</strong> coffee."),
        ("The shop ___ (close) at 6 PM.", "closes", "时刻表/习惯", "easy", "tense_present",
         "The shop <strong>closes</strong> at 6 PM."),
        ("Everyone ___ (love) her songs.", "loves", "不定代词作单数", "medium", "tense_present",
         "Everyone <strong>loves</strong> her songs."),
        ("Mathematics ___ (be) my favorite subject.", "is", "学科名词作单数", "medium", "tense_present",
         "Mathematics <strong>is</strong> my favorite subject."),
        ("The news ___ (be) very exciting.", "is", "news 不可数", "medium", "tense_present",
         "The news <strong>is</strong> very exciting."),
        ("Each student ___ (have) a desk.", "has", "each 单数", "medium", "tense_present",
         "Each student <strong>has</strong> a desk."),
        ("Nobody ___ (know) the answer.", "knows", "不定代词单数", "medium", "tense_present",
         "Nobody <strong>knows</strong> the answer."),
    ]

    for sent, ans, hint, d, cat, example in sp_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 现在进行时补充
    pc_more = [
        ("Shh! The baby ___ (sleep).", "is sleeping", "正在发生", "easy", "tense_present",
         "Shh! The baby <strong>is sleeping</strong>."),
        ("Why ___ (be) you crying?", "are", "进行时疑问", "easy", "tense_present",
         "Why <strong>are</strong> you crying?"),
        ("Look! A dog ___ (chase) a cat.", "is chasing", "Look!", "easy", "tense_present",
         "Look! A dog <strong>is chasing</strong> a cat."),
        ("I ___ (look for) my keys right now.", "am looking for", "right now", "easy", "tense_present",
         "I <strong>am looking for</strong> my keys right now."),
        ("She ___ (not work) this week.", "isn't working", "否定进行时", "easy", "tense_present",
         "She <strong>isn't working</strong> this week."),
        ("We ___ (plan) a trip these days.", "are planning", "these days", "medium", "tense_present",
         "We <strong>are planning</strong> a trip these days."),
        ("The world ___ (change) rapidly nowadays.", "is changing", "nowadays 趋势", "medium", "tense_present",
         "The world <strong>is changing</strong> rapidly nowadays."),
        ("He ___ (always/complain) about something!", "is always complaining", "总是（抱怨）", "hard", "tense_present",
         "He <strong>is always complaining</strong> about something!"),
        ("___ (be) they having a party?", "Are", "进行时疑问", "easy", "tense_present",
         "<strong>Are</strong> they having a party?"),
        ("My English ___ (improve) slowly.", "is improving", "渐进变化", "medium", "tense_present",
         "My English <strong>is improving</strong> slowly."),
    ]

    for sent, ans, hint, d, cat, example in pc_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 现在完成时补充
    pp_more = [
        ("I've already ___ (finish) reading it.", "finished", "already", "medium", "tense_present",
         "I've already <strong>finished</strong> reading it."),
        ("Have you ___ (be) to the Great Wall?", "been", "经历", "medium", "tense_present",
         "Have you <strong>been</strong> to the Great Wall?"),
        ("She hasn't ___ (find) her phone yet.", "found", "yet 否定", "medium", "tense_present",
         "She hasn't <strong>found</strong> her phone yet."),
        ("We've lived here ___ (for/since) 2015.", "since", "时间点用 since", "medium", "tense_present",
         "We've lived here <strong>since</strong> 2015."),
        ("I've waited ___ (for/since) two hours.", "for", "时间段用 for", "medium", "tense_present",
         "I've waited <strong>for</strong> two hours."),
        ("Is this the best food you've ever ___ (eat)?", "eaten", "最高级+完成时", "medium", "tense_present",
         "Is this the best food you've ever <strong>eaten</strong>?"),
        ("It's the most interesting book I've ever ___ (read).", "read", "最高级+完成时", "medium", "tense_present",
         "It's the most interesting book I've ever <strong>read</strong>."),
        ("He's just ___ (go) out.", "gone", "just", "medium", "tense_present",
         "He's just <strong>gone</strong> out."),
        ("Up to now, everything ___ (go) well.", "has gone", "up to now", "medium", "tense_present",
         "Up to now, everything <strong>has gone</strong> well."),
        ("I haven't seen him ___ (for/since) last week.", "since", "since+过去时间", "medium", "tense_present",
         "I haven't seen him <strong>since</strong> last week."),
        ("Have you heard from him ___ (recently/lately)? — Yes, I have.", "recently/lately", "最近", "medium", "tense_present",
         "Have you heard from him recently? — Yes, I have."),
        ("She's ___ (be) busy lately.", "been", "状态持续", "medium", "tense_present",
         "She's <strong>been</strong> busy lately."),
        ("They've already ___ (eat) dinner.", "eaten", "already", "medium", "tense_present",
         "They've already <strong>eaten</strong> dinner."),
        ("I've known her ___ (for/since) childhood.", "since", "since+时间点", "medium", "tense_present",
         "I've known her <strong>since</strong> childhood."),
        ("Has the train ___ (arrive) yet?", "arrived", "yet 疑问", "medium", "tense_present",
         "Has the train <strong>arrived</strong> yet?"),
        ("We haven't had any problems so ___ (far/much).", "far", "so far", "medium", "tense_present",
         "We haven't had any problems so <strong>far</strong>."),
        ("She's the kindest person I've ever ___ (meet).", "met", "最高级+完成时", "medium", "tense_present",
         "She's the kindest person I've ever <strong>met</strong>."),
        ("This is the first time I ___ (visit) Beijing.", "have visited", "第N次+完成时", "medium", "tense_present",
         "This is the first time I <strong>have visited</strong> Beijing."),
        ("He's already ___ (make) the bed.", "made", "already", "easy", "tense_present",
         "He's already <strong>made</strong> the bed."),
        ("They haven't ___ (decide) yet.", "decided", "yet", "easy", "tense_present",
         "They haven't <strong>decided</strong> yet."),
        ("I've ___ (spend) all my money.", "spent", "结果影响现在", "medium", "tense_present",
         "I've <strong>spent</strong> all my money."),
    ]

    for sent, ans, hint, d, cat, example in pp_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 一般过去时补充
    past_more = [
        ("I ___ (be) born in 2008.", "was", "be born", "easy", "tense_past",
         "I <strong>was</strong> born in 2008."),
        ("___ (do) you enjoy the party?", "Did", "Did+原形", "easy", "tense_past",
         "<strong>Did</strong> you enjoy the party?"),
        ("She didn't ___ (come) to school yesterday.", "come", "didn't+原形", "easy", "tense_past",
         "She didn't <strong>come</strong> to school yesterday."),
        ("We ___ (have) a great time last Sunday.", "had", "过去式 had", "easy", "tense_past",
         "We <strong>had</strong> a great time last Sunday."),
        ("When I was a child, I ___ (play) outside every day.", "played", "过去的习惯", "easy", "tense_past",
         "When I was a child, I <strong>played</strong> outside every day."),
        ("He ___ (tell) me a joke.", "told", "tell→told", "easy", "tense_past",
         "He <strong>told</strong> me a joke."),
        ("They ___ (go) to the zoo last week.", "went", "go→went", "easy", "tense_past",
         "They <strong>went</strong> to the zoo last week."),
        ("I ___ (see) a shooting star!", "saw", "see→saw", "easy", "tense_past",
         "I <strong>saw</strong> a shooting star!"),
        ("She ___ (buy) a new dress.", "bought", "buy→bought", "easy", "tense_past",
         "She <strong>bought</strong> a new dress."),
        ("We ___ (eat) pizza for lunch.", "ate", "eat→ate", "easy", "tense_past",
         "We <strong>ate</strong> pizza for lunch."),
        ("He ___ (give) me a gift.", "gave", "give→gave", "easy", "tense_past",
         "He <strong>gave</strong> me a gift."),
        ("They ___ (take) lots of photos.", "took", "take→took", "easy", "tense_past",
         "They <strong>took</strong> lots of photos."),
        ("I ___ (make) a cake.", "made", "make→made", "easy", "tense_past",
         "I <strong>made</strong> a cake."),
        ("She ___ (write) a letter.", "wrote", "write→wrote", "easy", "tense_past",
         "She <strong>wrote</strong> a letter."),
        ("The meeting ___ (begin) at 9 AM.", "began", "begin→began", "easy", "tense_past",
         "The meeting <strong>began</strong> at 9 AM."),
        ("Who ___ (break) the window?", "broke", "break→broke", "easy", "tense_past",
         "Who <strong>broke</strong> the window?"),
        ("I ___ (think) you were right.", "thought", "think→thought", "easy", "tense_past",
         "I <strong>thought</strong> you were right."),
        ("He ___ (leave) early.", "left", "leave→left", "easy", "tense_past",
         "He <strong>left</strong> early."),
        ("We ___ (meet) at the cafe.", "met", "meet→met", "easy", "tense_past",
         "We <strong>met</strong> at the cafe."),
        ("She ___ (say) goodbye.", "said", "say→said", "easy", "tense_past",
         "She <strong>said</strong> goodbye."),
        ("I ___ (feel) sad.", "felt", "feel→felt", "easy", "tense_past",
         "I <strong>felt</strong> sad."),
        ("He ___ (get) a new job.", "got", "get→got", "easy", "tense_past",
         "He <strong>got</strong> a new job."),
        ("Did you ___ (sleep) well?", "sleep", "Did+原形", "easy", "tense_past",
         "Did you <strong>sleep</strong> well?"),
        ("They ___ (bring) their umbrellas.", "brought", "bring→brought", "easy", "tense_past",
         "They <strong>brought</strong> their umbrellas."),
        ("I ___ (lose) my wallet.", "lost", "lose→lost", "easy", "tense_past",
         "I <strong>lost</strong> my wallet."),
        ("She ___ (send) me an email.", "sent", "send→sent", "easy", "tense_past",
         "She <strong>sent</strong> me an email."),
        ("We ___ (spend) the whole day there.", "spent", "spend→spent", "easy", "tense_past",
         "We <strong>spent</strong> the whole day there."),
        ("He ___ (pay) 50 yuan.", "paid", "pay→paid", "easy", "tense_past",
         "He <strong>paid</strong> 50 yuan."),
        ("I ___ (read) that book last month.", "read", "read 过去式", "easy", "tense_past",
         "I <strong>read</strong> that book last month."),
    ]

    for sent, ans, hint, d, cat, example in past_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 过去进行时补充
    past_cont_more = [
        ("What ___ (be) you doing at this time yesterday?", "were", "过去特定时刻", "medium", "tense_past",
         "What <strong>were</strong> you doing at this time yesterday?"),
        ("I ___ (take) a shower when you called.", "was taking", "when打断", "medium", "tense_past",
         "I <strong>was taking</strong> a shower when you called."),
        ("While I ___ (cook), the phone rang.", "was cooking", "while", "medium", "tense_past",
         "While I <strong>was cooking</strong>, the phone rang."),
        ("They ___ (play) cards all evening.", "were playing", "过去持续", "medium", "tense_past",
         "They were <strong>playing</strong> cards all evening."),
        ("She wasn't ___ (listen) to the teacher.", "listening", "否定", "medium", "tense_past",
         "She wasn't <strong>listening</strong> to the teacher."),
        ("___ (be) he sleeping when you came?", "Was", "疑问", "medium", "tense_past",
         "<strong>Was</strong> he sleeping when you came?"),
        ("It ___ (rain) heavily at that moment.", "was raining", "that moment", "medium", "tense_past",
         "It <strong>was raining</strong> heavily at that moment."),
        ("We ___ (wait) for the bus.", "were waiting", "等待中", "medium", "tense_past",
         "We <strong>were waiting</strong> for the bus."),
    ]

    for sent, ans, hint, d, cat, example in past_cont_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 过去完成时补充
    past_perf_more = [
        ("By the time I got there, the film ___ (start) already.", "had started", "过去的过去", "hard", "tense_past",
         "By the time I got there, the film <strong>had started</strong> already."),
        ("She said she ___ (see) the movie before.", "had seen", "间接引语", "hard", "tense_past",
         "She said she <strong>had seen</strong> the movie before."),
        ("By last Friday, we ___ (finish) the project.", "had finished", "by+过去时间", "hard", "tense_past",
         "By last Friday, we <strong>had finished</strong> the project."),
        ("I wished I ___ (know) earlier.", "had known", "wish+had done", "hard", "tense_past",
         "I wished I <strong>had known</strong> earlier."),
        ("He realized he ___ (leave) his wallet at home.", "had left", "先于主句", "hard", "tense_past",
         "He realized he <strong>had left</strong> his wallet at home."),
        ("It was the second time I ___ (visit) the museum.", "had visited", "第N次+had done", "hard", "tense_past",
         "It was the second time I <strong>had visited</strong> the museum."),
        ("After she ___ (finish) homework, she went out.", "had finished", "先后顺序", "hard", "tense_past",
         "After she <strong>had finished</strong> homework, she went out."),
        ("I didn't go because I ___ (not finish) my work.", "hadn't finished", "原因在主句前", "hard", "tense_past",
         "I didn't go because I hadn't <strong>finished</strong> my work."),
        ("By the age of 18, he ___ (learn) three languages.", "had learned", "by the age of", "hard", "tense_past",
         "By the age of 18, he <strong>had learned</strong> three languages."),
        ("She told me she ___ (live) in Shanghai for years.", "had lived", "持续到过去的点", "hard", "tense_past",
         "She told me she <strong>had lived</strong> in Shanghai for years."),
    ]

    for sent, ans, hint, d, cat, example in past_perf_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 将来时补充
    future_more = [
        ("I think she ___ (like) the gift.", "will like", "主观判断", "medium", "tense_future",
         "I think she <strong>will like</strong> the gift."),
        ("Look at those clouds! It ___ (rain).", "is going to rain", "迹象", "medium", "tense_future",
         "Look at those clouds! It <strong>is going to rain</strong>."),
        ("We ___ (visit) Grandma next weekend.", "are going to visit", "计划", "medium", "tense_future",
         "We <strong>are going to visit</strong> Grandma next weekend."),
        ("___ (do) you pass me the salt, please?", "Will/Would", "请求", "medium", "tense_future",
         "<strong>Will/Would</strong> you pass me the salt, please?"),
        ("I promise I ___ (not be) late.", "won't be", "承诺", "medium", "tense_future",
         "I promise I <strong>won't be</strong> late."),
        ("The train ___ (leave) in 5 minutes.", "is about to leave", "即将", "hard", "tense_future",
         "The train <strong>is about to leave</strong> in 5 minutes."),
        ("___ (shall) we dance?", "Shall", "提议", "medium", "tense_future",
         "<strong>Shall</strong> we dance?"),
        ("This time tomorrow I ___ (fly) to Tokyo.", "will be flying", "将来进行时", "hard", "tense_future",
         "This time tomorrow I <strong>will be flying</strong> to Tokyo."),
        ("By next year, she ___ (graduate) from college.", "will have graduated", "将来完成时", "hard", "tense_future",
         "By next year, she <strong>will have graduated</strong> from college."),
        ("Before you come back, I ___ (cook) dinner.", "will have cooked", "将来完成时", "hard", "tense_future",
         "Before you come back, I <strong>will have cooked</strong> dinner."),
        ("I ___ (be) 20 years old next birthday.", "will be", "将来事实", "easy", "tense_future",
         "I <strong>will be</strong> 20 years old next birthday."),
        ("Don't worry. I ___ (help) you.", 'll', "意愿", "easy", "tense_future",
         "Don't worry. I<strong>'ll help</strong> you."),
        ("The concert ___ (start) at 7 PM tonight.", "starts", "时刻表将来", "medium", "tense_future",
         "The concert <strong>starts</strong> at 7 PM tonight."),
        ("My aunt ___ (come) to see us tomorrow.", "is coming", "位移动词表将来", "medium", "tense_future",
         "My aunt <strong>is coming</strong> to see us tomorrow."),
    ]

    for sent, ans, hint, d, cat, example in future_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 条件句补充
    cond_more = [
        ("If it ___ (be) sunny, we will go hiking.", "is", "真实条件：主将从现", "medium", "tense_future",
         "If it <strong>is</strong> sunny, we will go hiking."),
        ("If you work hard, you ___ (pass) the exam.", "will pass", "主句将来", "medium", "tense_future",
         "If you work hard, you <strong>will pass</strong> the exam."),
        ("If I were a bird, I ___ (fly) freely.", "would fly", "虚拟现在", "hard", "tense_future",
         "If I were a bird, I <strong>would fly</strong> freely."),
        ("If I had money, I ___ (buy) a car.", "would buy", "虚拟现在", "hard", "tense_future",
         "If I had money, I would <strong>buy</strong> a car."),
        ("If I had studied harder, I ___ (pass) the exam.", "would have passed", "虚拟过去", "hard", "tense_future",
         "If I had studied harder, I would <strong>have passed</strong> the exam."),
        ("If she had set an alarm, she ___ (not be) late.", "would not have been", "虚拟过去", "hard", "tense_future",
         "If she had set an alarm, she <strong>would not have been</strong> late."),
        ("I wish I ___ (can) fly.", "could", "wish虚拟现在", "hard", "tense_future",
         "I wish I <strong>could</strong> fly."),
        ("I wish I ___ (not eat) so much.", "hadn't eaten", "wish虚拟过去", "hard", "tense_future",
         "I wish I hadn't <strong>eaten</strong> so much."),
        ("If it should rain, we ___ (cancel) the match.", "would/cancel will", "倒装/should", "hard", "tense_future",
         "If it should rain, we would cancel the match."),
        ("But for your help, I ___ (fail).", "would have failed", "含蓄条件", "hard", "tense_future",
         "But for your help, I would have <strong>failed</strong>."),
        ("Unless you hurry, you ___ (miss) the bus.", "will miss", "unless=if not", "medium", "tense_future",
         "Unless you hurry, you <strong>will miss</strong> the bus."),
        ("As long as you try, you ___ (succeed).", "will succeed", "as long as 条件", "medium", "tense_future",
         "As long as you try, you <strong>will succeed</strong>."),
        ("In case it rains, ___ (take) an umbrella.", "take", "in case", "medium", "tense_future",
         "In case it rains, <strong>take</strong> an umbrella."),
        ("Suppose/Supposing he ___ (refuse), what then?", "refuses/refused", "suppose假设", "hard", "tense_future",
         "Suppose he <strong>refuses/refused</strong>, what then?"),
        ("I would appreciate it if you ___ (reply) soon.", "replied", "委婉条件", "hard", "tense_future",
         "I would appreciate it if you <strong>replied</strong> soon."),
        ("If only I ___ (know) the answer!", "knew", "if only 虚拟现在", "hard", "tense_future",
         "If only I <strong>knew</strong> the answer!"),
        ("If only I ___ (not say) that!", "hadn't said", "if only 虚拟过去", "hard", "tense_future",
         "If only I hadn't <strong>said</strong> that!"),
        ("It is time we ___ (go) home.", "went/went", "it is time 虚拟", "hard", "tense_future",
         "It is time we went/were to <strong>go</strong> home."),
        ("I'd rather you ___ (come) tomorrow.", "came", "would rather 虚拟", "hard", "tense_future",
         "I'd rather you <strong>came</strong> tomorrow."),
    ]

    for sent, ans, hint, d, cat, example in cond_more:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # ===== 大量补充时态 MC 题目 =====
    extra_tense_mc = [
        # 一般现在时 MC
        ("The train ___ at 8 AM every day.", "leaves", ["will leave", "is leaving", "left"], "easy",
         "时刻表用一般现在时", ["The train <strong>leaves</strong> at 8 AM every day."],
         "时刻表 → 一般现在时"),
        ("___ you like ice cream?", "Do", ["Does", "Are", "Is"], "easy",
         "you 复数用 Do", ["<strong>Do</strong> you like ice cream?"],
         "Do you like...?"),
        ("He never ___ to school late.", "goes", ["go", "went", "going"], "easy",
         "never + 三单", ["He never <strong>goes</strong> to school late."],
         "never + 三单"),
        ("There ___ a book and two pens on the desk.", "is", ["are", "have", "has"], "medium",
         "there be 就近原则（a book 单数）", ["There <strong>is</strong> a book and two pens."],
         "就近原则 就近的单数"),
        ("Nobody ___ the answer.", "knows", ["know", "knew", "knowing"], "medium",
         "不定代词作单数", ["Nobody <strong>knows</strong> the answer."],
         "不定代词→三单"),
        ("Math ___ my favorite subject.", "is", ["are", "was", "were"], "medium",
         "学科名词作单数", ["Math <strong>is</strong> my favorite subject."],
         "学科名词单数"),
        ("Not only I but also she ___ wrong.", "is", ["am", "are", "be"], "hard",
         "not only...but also 就远原则", ["Not only I but also he <strong>is</strong> wrong."],
         "not only...but also 就远"),
        ("Either you or he ___ to go.", "has", ["have", "had", "having"], "hard",
         "either...or 就近原则", ["Either you or he <strong>has</strong> to go."],
         "either...or 就近"),
        ("The police ___ looking for him.", "are", ["is", "was", "be"], "hard",
         "police 集合名词常作复数", ["The police <strong>are</strong> investigating."],
         "police 复数"),

        # 现在进行时 MC
        ("Select the correct sentence:", "I am reading a book now.", ["I read a book now.", "I am read a book now.", "I have read a book now."], "easy",
         "now 提示进行时", ["I am <strong>reading</strong> a book now."],
         "now → be doing"),
        ("Why ___ you running?", "are", ["do", "is", "does"], "easy",
         "you 用 are", ["Why <strong>are</strong> you crying?"],
         "Are you doing?"),
        ("Look! The bus ___.", "is coming", ["comes", "came", "will come"], "easy",
         "Look! → 进行时", ["Look! The taxi <strong>is coming</strong>."],
         "Look! → is doing"),
        ("I'm tired. I ___ all day.", "have been working", ["am working", "work", "worked"], "hard",
         "解释原因用完成进行时", ["My eyes are red. I've been crying."],
         "解释原因 → have been doing"),
        ("She ___ for that company these days.", "is working", ["works", "worked", "has worked"], "medium",
         "these days → 进行时", ["He's learning French these days."],
         "these days → be doing"),

        # 现在完成时 MC
        ("I've already ___ the letter.", "written", ["wrote", "writing", "write"], "medium",
         "already + 过去分词", ["I've already finished it."],
         "already + done"),
        ("Have you ever ___ to a foreign country?", "been", ["gone", "went", "go"], "medium",
         "ever + been to 经历", ["Have you ever been to Japan?"],
         "ever + been to"),
        ("She hasn't completed her homework ___.", "yet", ["already", "just", "ever"], "medium",
         "yet 用于否定句末", ["Has he arrived yet?"],
         "yet 否定/疑问句"),
        ("I have lived here ___ 2010.", "since", ["for", "in", "at"], "medium",
         "since + 时间点", ["I've known him since childhood."],
         "since 时间点 / for 时间段"),
        ("It's the most beautiful place I ___.", "have ever visited", ["ever visit", "visited", "am visiting"], "medium",
         "最高级 + have ever done", ["She's the best teacher I've ever had."],
         "最高级 + have ever done"),
        ("This is the first time I ___ here.", "have come", ["come", "came", "do come"], "medium",
         "第N次 + have done", ["This is the second time I've made this mistake."],
         "第N次 + have done"),
        ("So far we ___ no news.", "have had", ["have", "had", "are having"], "medium",
         "so far → 完成时", ["So far everything goes well."],
         "so far → 完成"),
        ("— Have you seen my pen?\n— Yes, I ___ it on your desk.", "saw", ["have seen", "see", "am seeing"], "hard",
         "强调过去看到的动作而非现在的结果", ["I saw him yesterday."],
         "具体过去时间点用一般过去时"),

        # 一般过去时 MC
        ("I ___ to school by bus yesterday.", "went", ["go", "have gone", "was going"], "easy",
         "yesterday → 过去式", ["She came back last night."],
         "yesterday/last → went/did"),
        ("When ___ you born?", "were", ["are", "did", "was do"], "easy",
         "be born 用 was/were", ["Where were you born?"],
         "be born → was/were"),
        ("Did you enjoy the party? — Yes, I ___.", "did", ["do", "have", "am"], "easy",
         "Did 开头的一般疑问句回答也用 did", ["Did you finish? — Yes, I did."],
         "Did 问 Did 答"),
        ("He used to ___ up late, but not anymore.", "stay", ["stayed", "staying", "stays"], "medium",
         "used to do 过去常常", ["I used to play video games."],
         "used to do 过去常常"),
        ("What did you do last weekend? — I ___.", "stayed home", ["stay home", "staying home", "have stayed home"], "easy",
         "last weekend 回答用过去式", ["I visited my grandma."],
         "过去时间问 → 答过去时"),

        # 过去进行时 MC
        "split",

        # 过去完成时 MC
        ("When we got there, the film ___.", "had already started", ["already started", "has started", "was starting"], "hard",
         "过去的过去", ["When she came, I had finished dinner."],
         "过去的过去 had done"),
        ("By the time he was 20, he ___ 3 languages.", "had learned", ["learned", "has learned", "was learning"], "hard",
         "by + 过去时间 → had done", ["By last Friday, they'd completed it."],
         "by 过去时间 → had done"),
        ("She said she ___ the book before.", "had read", ["read", "has read", "reads"], "hard",
         "间接引语中的过去的过去", ["He told me he had met her before."],
         "间接引语 → had done"),
        ("I didn't know he ___ already ___.", "had;left", ["has;left", "did;leave", "was;leaving"], "hard",
         "不知道之前已发生的", ["I didn't know she had left."],
         "不知已发生 → had done"),

        # 将来时 MC
        ("I think it ___ rain tomorrow.", "will", ["shall", "is going to", "does"], "medium",
         "主观判断 will", ["I think she'll win."],
         "我认为会 → will"),
        ("Look at those clouds! It ___.", "is going to rain", ["will rain", "rains", "is raining"], "medium",
         "有迹象 → be going to", ["Watch out! You're going to fall!"],
         "有迹象 → be going to"),
        ("The plane ___ off in 20 minutes.", "takes", ["will take", "is taking", "took"], "medium",
         "时刻表/日程表用一般现在时表将来", ["The train leaves at 9."],
         "时刻表 → 一般现在时"),
        ("Don't worry. I ___ forget it.", "won't", ["don't", "didn't", "haven't"], "easy",
         "won't = will not 承诺/保证", ["I won't tell anyone."],
         "won't 承诺"),
        ("___ we dance?", "Shall", ["Will", "Would", "Should"], "medium",
         "Shall we 第一人称提议", ["Shall we go for a walk?"],
         "Shall we...?"),
        ("By 2030, I ___ this city for 20 years.", "will have lived", ["will live", "live", "have lived"], "hard",
         "by + 将来时间 → will have done", ["By next year, I'll have graduated."],
         "by将来 → will have done"),

        # 条件句 MC
        ("If it rains tomorrow, we ___ at home.", "will stay", ["stay", "would stay", "stayed"], "medium",
         "主将从现", ["If you try, you will succeed."],
         "真实条件 主将从现"),
        ("If I were you, I ___ his offer.", "would accept", ["will accept", "accept", "accepted"], "hard",
         "虚拟现在：if 过去，主句 would", ["If I were rich, I would travel."],
         "虚拟现在 would do"),
        ("If I had known, I ___ you.", "would have told", ["will tell", "would tell", "told"], "hard",
         "虚拟过去：if had done, would have done", ["If I had studied harder, I would have passed."],
         "虚拟过去 would have done"),
        ("I wish I ___ fly like a bird.", "could", ["can", "will can", "could have"], "hard",
         "wish + 过去时 与现在相反", ["I wish I were richer."],
         "wish 虚拟 could/were"),
        ("___ it rain, we will cancel.", "Should", ["Will", "Did", "Does"], "hard",
         "省略 if 的倒装", ["Were I you = If I were you"],
         "倒装 Should/Were/Had"),
        ("But for your help, I ___.", "would have failed", ["will fail", "fail", "failed"], "hard",
         "含蓄条件句", ["Without air, there would be no life."],
         "含蓄条件 would do"),
    ]

    i = 0
    while i < len(extra_tense_mc):
        item = extra_tense_mc[i]
        if item == "split":
            i += 1
            continue
        sent, ans, dist, d, exp, exs, mn = item
        o, ai = shuffle_options(dist + [ans], 3)
        # 根据 cat 分配
        if "since" in sent.lower() or "for" in sent.lower() or "already" in sent.lower() or "ever" in sent.lower() or "yet" in sent.lower() or "so far" in sent.lower() or "this is the" in sent.lower() or "most beautiful" in sent.lower():
            cat = "tense_present"
        elif "had " in ans or "had'" in ans.replace("'", "") or "by the time" in sent.lower() or "by the age" in sent.lower() or "by last" in sent.lower():
            cat = "tense_past"
        elif "will" in ans.lower() or "would" in ans.lower() or "shall" in ans.lower() or "going to" in ans.lower() or "should" in ans or "were" in ans or "but for" in sent.lower():
            cat = "tense_future"
        elif "yesterday" in sent.lower() or "last " in sent.lower() or "ago" in sent.lower() or "used to" in sent.lower() or "did " in sent.lower() or "born" in sent.lower() or "when were" in sent.lower():
            cat = "tense_past"
        elif "now" in sent.lower() or "look!" in sent.lower() or "listen!" in sent.lower() or "these days" in sent.lower() or "at the moment" in sent.lower() or "all day" in sent.lower() or "tired" in sent.lower():
            cat = "tense_present"
        elif "tomorrow" in sent.lower() or "next " in sent.lower() or "by 2030" in sent.lower() or "next year" in sent.lower() or "won't" in ans.lower() or "shall" in ans:
            cat = "tense_future"
        else:
            cat = "tense_present"

        qs.append(make_mc(nid(), cat, d, sent, "", o, ai, exp, exs, mn))
        i += 1

    # ===== 更多时态 Fill 题目 =====
    more_tense_fills = [
        # 一般现在时
        ("The earth ___ (go) around the sun.", "goes", "客观真理", "easy", "tense_present",
         "The earth <strong>goes</strong> around the sun."),
        ("___ (do) he like music?", "Does", "三单疑问 Does", "easy", "tense_present",
         "<strong>Does</strong> he like music?"),
        ("She ___ (not have) any brothers.", "doesn't have", "否定", "easy", "tense_present",
         "She <strong>doesn't have</strong> any brothers."),
        ("There ___ (be) a pen and two books.", "is", "就近原则", "medium", "tense_present",
         "There <strong>is</strong> a pen and two books."),
        ("Everyone ___ (want) to succeed.", "wants", "不定代词三单", "medium", "tense_present",
         "Everyone <strong>wants</strong> to succeed."),
        ("The news ___ (be) exciting!", "is", "news 不可数", "medium", "tense_present",
         "The news <strong>is</strong> exciting!"),
        ("Neither answer ___ (be) correct.", "is", "neither 单数", "hard", "tense_present",
         "Neither answer <strong>is</strong> correct."),

        # 现在进行时
        ("Be quiet! The baby ___ (sleep).", "is sleeping", "正在发生", "easy", "tense_present",
         "Be quiet! The baby <strong>is sleeping</strong>."),
        ("Why ___ (be) they laughing?", "are", "复数 are", "easy", "tense_present",
         "Why <strong>are</strong> they laughing?"),
        ("I ___ (look for) my glasses. Have you seen them?", "am looking for", "此刻进行", "easy", "tense_present",
         "I <strong>am looking for</strong> my glasses."),
        ("She ___ (work) on a new project these days.", "is working", "these days", "medium", "tense_present",
         "She <strong>is working</strong> on a new project these days."),
        ("You're always ___ (complain)!", "complaining", "always+进行(抱怨)", "hard", "tense_present",
         "You're always <strong>complaining</strong>!"),

        # 现在完成时
        ("I've already ___ (see) that movie.", "seen", "already+分词", "medium", "tense_present",
         "I've already <strong>seen</strong> that movie."),
        ("Has she ___ (finish) her work yet?", "finished", "yet疑问", "medium", "tense_present",
         "Has she <strong>finished</strong> her work yet?"),
        ("We've been friends ___ (since/for) primary school.", "since", "时间点 since", "medium", "tense_present",
         "We've been friends <strong>since</strong> primary school."),
        ("I've waited here ___ (since/for) an hour.", "for", "时间段 for", "medium", "tense_present",
         "I've waited here <strong>for</strong> an hour."),
        ("Is this the best meal you've ever ___ (have)?", "had", "最高级+完成", "medium", "tense_present",
         "Is this the best meal you've ever <strong>had</strong>?"),
        ("It's the first time I ___ (visit) China.", "have visited", "第N次+完成", "medium", "tense_present",
         "It's the first time I <strong>have visited</strong> China."),
        ("He's just ___ (go) out.", "gone", "just", "medium", "tense_present",
         "He's just <strong>gone</strong> out."),
        ("Up to now, everything ___ (go) well.", "has gone", "up to now", "medium", "tense_present",
         "Up to now, everything <strong>has gone</strong> well."),
        ("Have you heard from him ___ (recently)?", "recently", "recently", "medium", "tense_present",
         "Have you heard from him recently? — Yes, I have."),
        ("I haven't seen her ___ (since/for) last year.", "since", "since+过去时间", "medium", "tense_present",
         "I haven't seen her <strong>since</strong> last year."),
        ("They've already ___ (eat) dinner.", "eaten", "already", "easy", "tense_present",
         "They've already <strong>eaten</strong> dinner."),
        ("She hasn't ___ (decide) yet.", "decided", "yet 否定", "easy", "tense_present",
         "She hasn't <strong>decided</strong> yet."),
        ("I've ___ (spend) all my money.", "spent", "结果影响现在", "medium", "tense_present",
         "I've <strong>spent</strong> all my money."),

        # 一般过去时
        ("When ___ (be) you born?", "were", "be born 过去", "easy", "tense_past",
         "When <strong>were</strong> you born?"),
        ("___ (do) you have a good time?", "Did", "Did+原形", "easy", "tense_past",
         "<strong>Did</strong> you have a good time?"),
        ("She didn't ___ (come) yesterday.", "come", "didn't+原形", "easy", "tense_past",
         "She didn't <strong>come</strong> yesterday."),
        ("We ___ (have) fun at the beach last Sunday.", "had", "had fun", "easy", "tense_past",
         "We had fun at the beach last Sunday."),
        ("He ___ (tell) me a secret.", "told", "tell→told", "easy", "tense_past",
         "He told me a secret."),
        ("They ___ (go) home late.", "went", "go→went", "easy", "tense_past",
         "They went home late."),
        ("I ___ (make) a mistake.", "made", "make→made", "easy", "tense_past",
         "I made a mistake."),
        ("She ___ (buy) a gift for me.", "bought", "buy→bought", "easy", "tense_past",
         "She bought a gift for me."),
        ("Who ___ (break) the window?", "broke", "break→broke", "easy", "tense_past",
         "Who broke the window?"),
        ("I ___ (feel) happy.", "felt", "feel→felt", "easy", "tense_past",
         "I felt happy."),
        ("We ___ (meet) at the station.", "met", "meet→met", "easy", "tense_past",
         "We met at the station."),
        ("He ___ (give) me a book.", "gave", "give→gave", "easy", "tense_past",
         "He gave me a book."),
        ("She ___ (write) a letter.", "wrote", "write→wrote", "easy", "tense_past",
         "She wrote a letter."),
        ("They ___ (take) many photos.", "took", "take→took", "easy", "tense_past",
         "They took many photos."),
        ("I ___ (leave) early.", "left", "leave→left", "easy", "tense_past",
         "I left early."),
        ("Did you ___ (sleep) well?", "sleep", "Did+原形", "easy", "tense_past",
         "Did you sleep well?"),
        ("We ___ (spend) all day there.", "spent", "spend→spent", "easy", "tense_past",
         "We spent all day there."),
        ("He ___ (pay) 100 yuan.", "paid", "pay→paid", "easy", "tense_past",
         "He paid 100 yuan."),
        ("I ___ (lose) my key.", "lost", "lose→lost", "easy", "tense_past",
         "I lost my key."),
        ("She ___ (send) an email.", "sent", "send→sent", "easy", "tense_past",
         "She sent an email."),
        ("They ___ (bring) some food.", "brought", "bring→brought", "easy", "tense_past",
         "They brought some food."),

        # 过去进行时
        ("What were you doing when I ___ (call)?", "called", "when打断", "medium", "tense_past",
         "What were you doing when I called?"),
        ("While Mom ___ (cook), Dad read a paper.", "was cooking", "while", "medium", "tense_past",
         "While Mom was cooking, Dad read a paper."),
        ("It ___ (rain) at that time.", "was raining", "that time", "medium", "tense_past",
         "It was raining at that time."),
        ("She wasn't ___ (listen).", "listening", "否定", "medium", "tense_past",
         "She wasn't listening."),
        ("___ (be) they playing football?", "Were", " Were+doing", "medium", "tense_past",
         "Were they playing football?"),

        # 过去完成时
        ("By the time I got there, she ___.", "had left", "过去的过去", "hard", "tense_past",
         "By the time I got there, she had left."),
        ("He said he ___ (see) the film before.", "had seen", "间接引语", "hard", "tense_past",
         "He said he had seen the film before."),
        ("By last Friday, we ___ it.", "had finished", "by过去", "hard", "tense_past",
         "By last Friday, we'd finished it."),
        ("I wished I ___ (know) earlier.", "had known", "wish虚拟", "hard", "tense_past",
         "I wished I had known earlier."),
        ("After she ___ (eat), she went out.", "had eaten", "先后顺序", "hard", "tense_past",
         "After she'd eaten, she went out."),
        ("I didn't go because I ___ (not finish).", "hadn't finished", "原因在前", "hard", "tense_past",
         "I didn't go because I hadn't finished."),
        ("By age 18, he ___ three languages.", "had learned", "by the age", "hard", "tense_past",
         "By age 18, he'd learned three languages."),
        ("She said she ___ (live) there for years.", "had lived", "持续到过去", "hard", "tense_past",
         "She said she'd lived there for years."),

        # 一般将来时
        ("I think she ___ (like) it.", "will like", "主观判断", "medium", "tense_future",
         "I think she'll like it."),
        ("Those clouds mean it ___.", "is going to rain", "迹象", "medium", "tense_future",
         "Those clouds mean it is going to rain."),
        ("We ___ (visit) them next Sunday.", "are going to visit", "计划", "medium", "tense_future",
         "We're going to visit them next Sunday."),
        ("___ you pass me that salt?", "Will/Would", "请求", "medium", "tense_future",
         "Will/Would you pass me that salt?"),
        ("I promise I ___ (not tell) anyone.", "won't tell", "承诺", "medium", "tense_future",
         "I promise I won't tell anyone."),
        ("The show ___ (start) soon.", "is about to start", "即将", "hard", "tense_future",
         "The show is about to start soon."),
        ("___ we take a break?", "Shall", "提议", "medium", "tense_future",
         "Shall we take a break?"),
        ("This time tomorrow I ___ (fly) to Paris.", "will be flying", "将来进行", "hard", "tense_future",
         "This time tomorrow I will be flying to Paris."),
        ("By next month, she ___ (finish) the book.", "will have finished", "将来完成", "hard", "tense_future",
         "By next month, she will have finished the book."),
        ("I ___ (be) 18 next birthday.", "will be", "将来事实", "easy", "tense_future",
         "I will be 18 next birthday."),
        ("Don't worry. I ___ (help) you.", "'ll/will", "意愿", "easy", "tense_future",
         "Don't worry. I'll help you."),
        ("My sister ___ (come) tomorrow.", "is coming", "位移动词表将来", "medium", "tense_future",
         "My sister is coming tomorrow."),

        # 条件句
        ("If it ___ (be) fine, we'll go hiking.", "is", "真实条件", "medium", "tense_future",
         "If it is fine, we'll go hiking."),
        ("If you try hard, you ___ (succeed).", "will succeed", "主将从现主句", "medium", "tense_future",
         "If you try hard, you will succeed."),
        ("If I were rich, I ___ (travel) around the world.", "would travel", "虚拟现在", "hard", "tense_future",
         "If I were rich, I would travel around the world."),
        ("If I had time, I ___ (help) you.", "would help", "虚拟现在", "hard", "tense_future",
         "If I had time, I would help you."),
        ("If I had studied harder, I ___ (pass).", "would have passed", "虚拟过去", "hard", "tense_future",
         "If I had studied harder, I would have passed."),
        ("If she had come earlier, she ___ (not miss) the bus.", "wouldn't have missed", "虚拟过去", "hard", "tense_future",
         "If she had come earlier, she wouldn't have missed the bus."),
        ("I wish I ___ (can) fly.", "could", "wish虚拟", "hard", "tense_future",
         "I wish I could fly."),
        ("I wish I ___ (not say) that!", "hadn't said", "wish虚拟过去", "hard", "tense_future",
         "I wish I hadn't said that!"),
        ("If it should rain, we ___ (cancel).", "will/would cancel", "should倒装", "hard", "tense_future",
         "If it should rain, we will cancel."),
        ("But for your help, we ___ (fail).", "would have failed", "含蓄条件", "hard", "tense_future",
         "But for your help, we would have failed."),
        ("Unless you hurry, you ___ (be) late.", "will be", "unless=if not", "medium", "tense_future",
         "Unless you hurry, you will be late."),
        ("___ (suppose) he refuses, what then?", "Suppose/Supposing", "suppose假设", "hard", "tense_future",
         "Suppose he refuses, what then?"),
        ("I'd rather you ___ (come) tomorrow.", "came", "would rather 虚拟", "hard", "tense_future",
         "I'd rather you came tomorrow."),
        ("It's time we ___ (go) home.", "went", "it's time 虚拟", "hard", "tense_future",
         "It's time we went home."),
    ]

    for sent, ans, hint, d, cat, example in more_tense_fills:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # ===== 最后一批时态 MC 题目 =====
    final_tense_mc = [
        # 一般现在时
        ("He ___ (go) to school by bus every day.", "goes", ["go", "going", "gone"], "easy",
         "三单加es", ["He goes to school by bus."], ""),
        ("Water ___ (boil) at 100°C.", "boils", ["is boiling", "boiled", "has boiled"], "easy",
         "科学事实", ["Water boils at 100°C."], ""),
        ("She usually ___ (get) up at 6:30.", "gets", ["getting", "got", "is getting"], "easy",
         "usually + 三单", ["She gets up at 6:30."], ""),
        ("___ (do) they live here?", "Do", ["Does", "Are", "Is"], "easy",
         "they 复数用 Do", ["Do they live here?"], ""),

        # 现在进行时
        ("Listen! Someone ___ (knock) at the door.", "is knocking", ["knocks", "knocked", "has knocked"], "easy",
         "Listen! → 进行时", ["Listen! Someone is knocking."], ""),
        ("I ___ (not enjoy) this party.", "am not enjoying", ["don't enjoy", "enjoy", "didn't enjoy"], "medium",
         "此刻的状态", ["I'm not enjoying this party."], ""),
        ("___ (be) she working on it now?", "Is", ["Does", "Has", "Was"], "easy",
         "Is she doing?", ["Is she working?"], ""),

        # 现在完成时
        ("I've already ___ (finish) my work.", "finished", ["finishing", "finish", "finishes"], "medium",
         "already + 过去分词", ["I've already finished."], ""),
        ("Have you ever ___ (be) abroad?", "been", ["gone", "be", "being"], "medium",
         "have been to 经历", ["Have you ever been abroad?"], ""),
        ("She hasn't called me ___.", "yet", ["already", "just", "since"], "medium",
         "yet 否定句末尾", ["She hasn't called yet."], ""),
        ("We've known each other ___ ten years.", "for", ["since", "in", "during"], "medium",
         "for + 时间段", ["for ten years"], ""),

        # 一般过去时
        ("I ___ (see) him at the party.", "saw", ["have seen", "see", "seeing"], "easy",
         "过去时间用 saw", ["I saw him there."], ""),
        ("Did you ___ (enjoy) the movie?", "enjoy", ["enjoyed", "enjoying", "to enjoy"], "easy",
         "Did + 原形", ["Did you enjoy it?"] , ""),
        ("She wasn't at home yesterday. She ___ (go) out.", "went", ["has gone", "goes", "going"], "easy",
         "yesterday → went", ["She went out."], ""),

        # 过去进行时
        ("I ___ (take) a shower when the phone rang.", "was taking", ["took", "have taken", "took"], "medium",
         "长动作被 when 打断", ["I was taking a shower."], ""),
        ("What were you doing when I ___ (call)?", "called", ["was calling", "call", "am calling"], "medium",
         "when 从句用一般过去时", ["When I called..."], ""),

        # 过去完成时
        ("By the time he arrived, we ___.", "had left", ["left", "have left", "were leaving"], "hard",
         "过去的过去", ["By then, we had left."], ""),
        ("She realized she ___ (leave) her bag at home.", "had left", ["left", "has left", "leaves"], "hard",
         "先于主句动作", ["She'd left her bag."], ""),

        # 将来时
        ("Look at the sky! It ___.", "is going to rain", ["will rain", "rains", "is raining"], "medium",
         "有迹象", ["It's going to rain."], ""),
        ("I promise I ___ (not be) late again.", "won't be", ["don't be", "am not", "haven't been"], "easy",
         "承诺 won't", ["I won't be late."], ""),
        ("The train ___ (leave) in five minutes.", "is about to leave", ["will leave", "leaves", "left"], "hard",
         "即将 is about to", ["It's about to leave."], ""),
        ("By 2027, I ___ (graduate) from college.", "will have graduated", ["graduate", "will graduate", "have graduated"], "hard",
         "by 将来完成", ["By then I will have graduated."], ""),

        # 条件句
        ("If it rains tomorrow, we ___ (cancel).", "will cancel", ["cancel", "would cancel", "cancelled"], "medium",
         "主将从现", ["If it rains, we'll cancel."], ""),
        ("If I were you, I ___ (accept) it.", "would accept", ["accept", "will accept", "accepted"], "hard",
         "虚拟现在 would do", ["If I were you, I'd accept."], ""),
        ("If I had known, I ___ (tell) you.", "would have told", ["would tell", "had told", "told"], "hard",
         "虚拟过去 would have done", ["I would have told you."], ""),
        ("I wish I ___ (can) help you.", "could", ["can", "will can", "could have"], "hard",
         "wish 虚拟 could", ["I wish I could help."], ""),
    ]

    for sent, ans, dist, d, exp, exs, mn in final_tense_mc:
        o, ai = shuffle_options(dist + [ans], 3)
        if "since" in sent or "already" in sent or "ever" in sent or "yet" in sent or "so far" in sent or "this is the" in sent.lower() or "most" in sent.lower():
            cat = "tense_present"
        elif "had " in ans or "had'" in ans.replace("'", "") or "by the time" in sent.lower() or "by last" in sent.lower() or "realized" in sent.lower():
            cat = "tense_past"
        elif "will" in ans.lower() or "would" in ans.lower() or "going to" in ans.lower() or "won't" in ans.lower() or "promise" in sent.lower() or "about to" in sent.lower() or "look at" in sent.lower() and "sky" in sent.lower() or "by 2027" in sent.lower():
            cat = "tense_future"
        elif "yesterday" in sent.lower() or "did you" in sent.lower() or "wasn't" in sent.lower() or "last night" in sent.lower() or "was taking" in ans or "when the phone rang" in sent.lower():
            cat = "tense_past"
        elif "listen!" in sent.lower() or "now" in sent.lower() or "working on it now" in sent.lower() or "knock" in sent.lower():
            cat = "tense_present"
        elif "tomorrow" in sent.lower() or "rains tomorrow" in sent.lower():
            cat = "tense_future"
        else:
            cat = "tense_present"
        qs.append(make_mc(nid(), cat, d, sent, "", o, ai, exp, exs, mn))

    # ===== 最终补充时态 MC（最后50+题）=====
    final_batch = [
        # 一般现在时
        ("The shop ___ (open) at 9 AM.", "opens", ["is opening", "opened", "has opened"], "easy",
         "时刻表", ["The shop opens at 9."], ""),
        ("He ___ (not like) coffee.", "doesn't like", ["don't like", "isn't liking", "didn't like"], "easy",
         "三单否定", ["He doesn't like coffee."], ""),
        ("How often ___ (do) you exercise?", "do", ["does", "are", "is"], "easy",
         "you 用 do", ["How often do you...?"], ""),

        # 现在进行时
        ("I can't come now. I ___ (work).", "'m working", ["work", "worked", "have worked"], "easy",
         "now → 进行", ["I'm working now."], ""),
        ("She ___ (currently/study) French.", "is currently studying", ["studies", "studied", "has studied"], "medium",
         "currently → 进行", ["She's studying French."], ""),

        # 现在完成时
        ("I've just ___ (have) lunch.", "had", ["having", "have", "has"], "medium",
         "just + 过去分词", ["I've just had lunch."], ""),
        ("Have you ___ (read) this book?", "read", ["reading", "reads", "to read"], "medium",
         "read 的过去分词 read", ["Have you read it?"], ""),
        ("We haven't decided where ___ (go) for holiday.", "to go", ["going", "go", "went"], "hard",
         "疑问词 + 不定式", ["where to go"], ""),

        # 一般过去时
        ("When I was a child, I ___ (play) outside every day.", "played", ["have played", "was playing", "play"], "easy",
         "回忆过去习惯", ["When I was young, I played."], ""),
        ("___ (be) you at home last night?", "Were", ["Are", "Did", "Was"], "easy",
         "you 复数 Were", ["Were you at home?"], ""),
        ("How did you ___ (find) the exam?", "find", ["found", "finding", "to find"], "easy",
         "did + 原形", ["did you find?"], ""),

        # 过去进行时
        ("I was walking home when it ___ (start) to rain.", "started", ["was starting", "has started", "starts"], "medium",
         "when 从句一般过去", ["when it started"], ""),
        ("They were having dinner when I ___ (arrive).", "arrived", ["was arriving", "have arrived", "arrive"], "medium",
         "短动作打断长动作", ["when I arrived"], ""),

        # 过去完成时
        ("She told me she ___ (already/finish).", "had already finished", ["already finished", "has finished", "finishes"], "hard",
         "间接引语过去的过去", ["she'd finished"], ""),
        ("I didn't recognize him. He ___ (change) a lot.", "had changed", ["changed", "has changed", "changes"], "hard",
         "变化在认不出之前发生", ["he'd changed"], ""),

        # 将来时
        ("I think they ___ (win) the game.", "will win", ["would win", "win", "won"], "medium",
         "主观判断 will", ["I think they'll win."], ""),
        ("Don't worry. The doctor ___ (be) here soon.", "will be", ["is", "was", "has been"], "easy",
         "将来 will be", ["will be here soon"], ""),
        ("We ___ (fly) to Tokyo next Monday.", "are flying", ["will fly", "flew", "flying"], "medium",
         "位移动词 be doing 表将来", ["We're flying next week."], ""),

        # 条件句
        ("If you heat ice, it ___.", "melts", ["will melt", "would melt", "melted"], "medium",
         "客观真实条件句用一般现在时（即使主句也可省略）", ["If you heat ice, it melts."], ""),
        ("If I were in your position, I ___ (accept) the offer.", "would accept", ["accept", "will accept", "accepted"], "hard",
         "虚拟现在 would do", ["If I were you, I'd accept."], ""),
        ("If she had studied harder, she ___ (pass) the exam.", "would have passed", ["passed", "would pass", "has passed"], "hard",
         "虚拟过去 would have done", ["would have passed"], ""),
    ]

    for sent, ans, dist, d, exp, exs, mn in final_batch:
        o, ai = shuffle_options(dist + [ans], 3)
        if any(w in sent.lower() for w in ["since","already","ever","yet","just have","so far"]):
            cat = "tense_present"
        elif any(w in ans for w in ["had ","would have"]) or "by the time" in sent or "realized" in sent or "told me" in sent or "didn't recognize" in sent:
            cat = "tense_past"
        elif any(w in ans for w in ["will ","would 'd ","going to","'m flying","won't"]) or "tomorrow" in sent.lower() or "next " in sent.lower() or "soon" in sent.lower() or "think they" in sent.lower() or "don't worry" in sent.lower():
            cat = "tense_future"
        elif any(w in sent.lower() for w in ["yesterday","last night","when i was a child","were you at","how did"]):
            cat = "tense_past"
        elif any(w in sent.lower() for w in ["now","can't come","currently","listen!","look!","every day"]):
            cat = "tense_present"
        elif "if you heat" in sent.lower() or "were in your" in sent.lower() or "if she had studied" in sent.lower():
            cat = "tense_future"
        else:
            cat = "tense_present"
        qs.append(make_mc(nid(), cat, d, sent, "", o, ai, exp, exs, mn))

    # ===== 最后补充到 500+ =====
    last_batch_fills = [
        ("He ___ (usually/walk) to school.", "usually walks", "三单", "easy", "tense_present",
         "He usually walks to school."),
        ("___ (be) there any milk?", "Is", "milk 不可数", "easy", "tense_present",
         "<strong>Is</strong> there any milk?"),
        ("The class ___ (begin) at 8 sharp.", "begins", "时刻表", "easy", "tense_present",
         "The class begins at 8 sharp."),
        ("She ___ (not watch) TV on weekdays.", "doesn't watch", "否定", "easy", "tense_present",
         "She doesn't watch TV on weekdays."),
        ("How ___ (do) he go to work?", "does", "三单疑问", "easy", "tense_present",
         "How does he go to work?"),
        ("Look! It ___ (snow)!", "is snowing", "Look!", "easy", "tense_present",
         "Look! It is snowing!"),
        ("I ___ (wait) for you at the moment.", "am waiting", "at the moment", "easy", "tense_present",
         "I am waiting for you at the moment."),
        ("They ___ (plan) a trip for the holiday.", "are planning", "计划中", "medium", "tense_present",
         "They are planning a trip for the holiday."),
        ("I've ___ (already/see) that film.", "already seen", "already", "medium", "tense_present",
         "I've already seen that film."),
        ("Have you ___ (ever/be) to London?", "ever been", "ever been", "medium", "tense_present",
         "Have you ever been to London?"),
        ("She hasn't called me ___.", "yet", "yet 否定句", "medium", "tense_present",
         "She hasn't called me yet."),
        ("We've lived here ___ five years.", "for", "for + 时间段", "medium", "tense_present",
         "We've lived here for five years."),
        ("I ___ (go) to the park yesterday.", "went", "yesterday", "easy", "tense_past",
         "I went to the park yesterday."),
        ("Did she ___ (have) a good time?", "have", "Did+原形", "easy", "tense_past",
         "Did she have a good time?"),
        ("I didn't ___ (know) the answer.", "know", "didn't+原形", "easy", "tense_past",
         "I didn't know the answer."),
        ("What were you doing when I ___ (call)?", "called", "when+过去式", "medium", "tense_past",
         "What were you doing when I called?"),
        ("While I ___ (cook), the phone rang.", "was cooking", "while 进行时", "medium", "tense_past",
         "While I was cooking, the phone rang."),
        ("By the time I got home, Mom ___ already ___.", "had cooked; dinner", "过去的过去", "hard", "tense_past",
         "By the time I got home, Mom had already cooked dinner."),
        ("I think it ___ (rain) later.", "will rain", "will 判断", "medium", "tense_future",
         "I think it will rain later."),
        ("Those dark clouds mean it is going to ___.", "rain", "迹象 be going to", "medium", "tense_future",
         "Those dark clouds mean it is going to rain."),
        ("I promise I ___ (not tell) anyone.", "won't tell", "承诺 won't", "medium", "tense_future",
         "I promise I won't tell anyone."),
        ("If it rains tomorrow, we ___ (stay) home.", "will stay", "主将从现主句", "medium", "tense_future",
         "If it rains, we'll stay home."),
        ("If I were you, I ___ (take) the job.", 'would take', "虚拟现在 would do", "hard", "tense_future",
         "If I were you, I'd take the job."),
        ("If I had known, I ___ (come).", "would have come", "虚拟过去 would have done", "hard", "tense_future",
         "If I had known, I would have come."),
        ("I wish I ___ (can) help.", "could", "wish 虚拟 could", "hard", "tense_future",
         "I wish I could help."),
        ("Unless you hurry, you ___ (miss) the bus.", "will miss", "unless 主将", "medium", "tense_future",
         "Unless you hurry, you'll miss the bus."),
        ("It's high time we ___ (leave).", "left", "it's time 虚拟过去", "hard", "tense_future",
         "It's high time we left."),
    ]

    for sent, ans, hint, d, cat, example in last_batch_fills:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    # 补足最后几题到500+
    final_topup = [
        ("I ___ (be) tired today.", "am", "现在状态", "easy", "tense_present",
         "I am tired today."),
        ("What will you do if it ___ (rain) tomorrow?", "rains", "if从句现在时", "medium", "tense_future",
         "What will you do if it rains tomorrow?"),
        ("She said she ___ (can) come.", "could", "间接引语情态动词过去", "hard", "tense_past",
         "She said she could come."),
    ]
    for sent, ans, hint, d, cat, example in final_topup:
        qs.append(make_fill(nid(), cat, d, sent, hint, ans, "", [example], ""))

    return qs


# ============================================================
# 主程序
# ============================================================

def main():
    global next_id

    print("=" * 60)
    print("初中生背单词 - 动词与时态题库生成器")
    print("=" * 60)

    # ---- 生成 verbs.json ----
    print("\n[1/2] 生成动词题库 (verbs.json)...")

    # 重置 ID
    next_id = [1]

    verbs_questions = []
    verbs_questions.extend(gen_irregular_verbs())
    print(f"  不规则动词: {len(gen_irregular_verbs()) if False else '?'} 题")

    count_irregular = len(verbs_questions)
    print(f"  [-] 不规则动词过去式/过去分词: {count_irregular} 题")

    t_count = len(verbs_questions)
    verbs_questions.extend(gen_transitive_intransitive())
    print(f"  [-] 及物/不及物动词: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    verbs_questions.extend(gen_verb_pairs())
    print(f"  [-] 高频动词辨析: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    verbs_questions.extend(gen_make_vs_do())
    print(f"  [-] make vs do 搭配: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    verbs_questions.extend(gen_have_take_get_collocations())
    print(f"  [-] have/take/get 搭配: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    verbs_questions.extend(gen_modal_verbs())
    print(f"  [-] 情态动词: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    verbs_questions.extend(gerund_infinitive_data())
    print(f"  [-] 动词+doing vs +to do: {len(verbs_questions) - t_count} 题")
    t_count = len(verbs_questions)

    # 补充更多动词题
    verbs_questions.extend(gen_extra_verbs())
    print(f"  [-] 补充动词题: {len(verbs_questions) - t_count} 题")

    total_verbs = len(verbs_questions)
    print(f"\n  动词题库总计: {total_verbs} 题")

    # 写入 verbs.json
    verbs_path = os.path.join(DATA_DIR, "verbs.json")
    with open(verbs_path, "w", encoding="utf-8") as f:
        json.dump(verbs_questions, f, ensure_ascii=False, indent=2)
    print(f"  已保存: {verbs_path}")

    # ---- 生成 tenses.json ----
    print("\n[2/2] 生成时态题库 (tenses.json)...")

    # 重置 ID
    next_id = [1]

    tenses_questions = []
    tenses_questions.extend(gen_simple_present())
    sp_len = len(tenses_questions)
    print(f"  [-] 一般现在时: {sp_len} 题")

    tenses_questions.extend(gen_present_continuous())
    print(f"  [-] 现在进行时: {len(tenses_questions) - sp_len} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_present_perfect())
    print(f"  [-] 现在完成时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_present_perf_cont())
    print(f"  [-] 现在完成进行时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_simple_past())
    print(f"  [-] 一般过去时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_past_continuous())
    print(f"  [-] 过去进行时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_past_perfect())
    print(f"  [-] 过去完成时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_simple_future())
    print(f"  [-] 一般将来时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_future_continuous())
    print(f"  [-] 将来进行时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_future_perfect())
    print(f"  [-] 将来完成时: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_conditionals())
    print(f"  [-] 条件句时态搭配: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    tenses_questions.extend(gen_more_tense_questions())
    print(f"  [-] 补充时态题目: {len(tenses_questions) - t_count} 题")
    t_count = len(tenses_questions)

    # 补充更多时态题
    tenses_questions.extend(gen_extra_tenses())
    print(f"  [-] 大量补充时态题: {len(tenses_questions) - t_count} 题")

    total_tenses = len(tenses_questions)
    print(f"\n  时态题库总计: {total_tenses} 题")

    # 写入 tenses.json
    tenses_path = os.path.join(DATA_DIR, "tenses.json")
    with open(tenses_path, "w", encoding="utf-8") as f:
        json.dump(tenses_questions, f, ensure_ascii=False, indent=2)
    print(f"  已保存: {tenses_path}")

    # ---- 统计信息 ----
    print("\n" + "=" * 60)
    print("统计信息")
    print("=" * 60)

    # verbs 统计
    v_easy = sum(1 for q in verbs_questions if q["diff"] == "easy")
    v_med = sum(1 for q in verbs_questions if q["diff"] == "medium")
    v_hard = sum(1 for q in verbs_questions if q["diff"] == "hard")
    v_mc = sum(1 for q in verbs_questions if q["type"] == "mc")
    v_fill = sum(1 for q in verbs_questions if q["type"] == "fill")

    print(f"\nverbs.json:")
    print(f"  总题数:   {total_verbs}")
    print(f"  easy:     {v_easy} ({v_easy/total_verbs*100:.1f}%)")
    print(f"  medium:   {v_med} ({v_med/total_verbs*100:.1f}%)")
    print(f"  hard:     {v_hard} ({v_hard/total_verbs*100:.1f}%)")
    print(f"  mc:       {v_mc} ({v_mc/total_verbs*100:.1f}%)")
    print(f"  fill:     {v_fill} ({v_fill/total_verbs*100:.1f}%)")

    # tenses 统计
    t_easy = sum(1 for q in tenses_questions if q["diff"] == "easy")
    t_med = sum(1 for q in tenses_questions if q["diff"] == "medium")
    t_hard = sum(1 for q in tenses_questions if q["diff"] == "hard")
    t_mc = sum(1 for q in tenses_questions if q["type"] == "mc")
    t_fill = sum(1 for q in tenses_questions if q["type"] == "fill")

    # 按 cat 分类
    t_cats = {}
    for q in tenses_questions:
        c = q["cat"]
        t_cats[c] = t_cats.get(c, 0) + 1

    print(f"\ntenses.json:")
    print(f"  总题数:   {total_tenses}")
    print(f"  easy:     {t_easy} ({t_easy/total_tenses*100:.1f}%)")
    print(f"  medium:   {t_med} ({t_med/total_tenses*100:.1f}%)")
    print(f"  hard:     {t_hard} ({t_hard/total_tenses*100:.1f}%)")
    print(f"  mc:       {t_mc} ({t_mc/total_tenses*100:.1f}%)")
    print(f"  fill:     {t_fill} ({t_fill/total_tenses*100:.1f}%)")
    print(f"  时态分布:")
    for cat, cnt in sorted(t_cats.items()):
        print(f"    {cat}: {cnt}")

    print("\n生成完成!")


if __name__ == "__main__":
    main()
