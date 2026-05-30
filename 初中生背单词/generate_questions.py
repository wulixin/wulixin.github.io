#!/usr/bin/env python3
"""
初中英语词汇闯关系统 — 大规模题库生成器
目标：每个类型 400-600 题，共 10 类约 5000 题
"""

import json
import random
import re
import os
from collections import defaultdict

random.seed(42)

OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ============================================================
# 全局题目 ID 计数器
# ============================================================
id_counter = [1]

def next_id():
    val = id_counter[0]
    id_counter[0] += 1
    return val

# ============================================================
# 工具函数
# ============================================================
def make_mc(q, hint, options, answer_idx, explain, examples, mnemonic, cat, diff):
    """生成选择题"""
    return {
        "id": next_id(),
        "cat": cat,
        "diff": diff,
        "type": "mc",
        "q": q,
        "hint": hint,
        "options": options,
        "answer": answer_idx,
        "explain": explain,
        "examples": examples,
        "mnemonic": mnemonic
    }

def make_fill(q, hint, answer, explain, examples, mnemonic, cat, diff):
    """生成填空题"""
    return {
        "id": next_id(),
        "cat": cat,
        "diff": diff,
        "type": "fill",
        "q": q,
        "hint": hint,
        "answer": answer,
        "explain": explain,
        "examples": examples,
        "mnemonic": mnemonic
    }

# ============================================================
# 1. PREPOSITIONS 介词题库 (目标 500 题)
# ============================================================
def generate_prepositions():
    questions = []

    # --- 时间介词 ---
    time_preps = [
        ("at", "具体时刻", ["at 7:30", "at noon", "at midnight", "at dawn", "at sunset", "at breakfast", "at Christmas", "at the weekend"]),
        ("on", "具体某天/星期", ["on Monday", "on Tuesday morning", "on May 1st", "on my birthday", "on New Year's Day", "on the weekend(美)", "on a cold morning", "on the first day"]),
        ("in", "月份/年份/季节", ["in May", "in 2023", "in summer", "in the morning", "in the 21st century", "in the past", "in the future", "in my childhood"]),
        ("for", "持续一段时间", ["for two hours", "for three days", "for a long time", "for ages", "for the whole week", "for 10 years"]),
        ("since", "从某时间点起", ["since 2019", "since last Monday", "since childhood", "since then", "since 8 o'clock", "since the war"]),
        ("during", "在…期间", ["during the meeting", "during the summer", "during my stay", "during the war", "during the night", "during class"]),
        ("by", "到…之前/不晚于", ["by Friday", "by 5 p.m.", "by the end of", "by next week", "by the time", "by now"]),
        ("until/till", "直到…为止", ["until midnight", "until next week", "until the end", "until now", "till tomorrow", "until further notice"]),
        ("before", "在…之前", ["before dinner", "before the exam", "before sunrise", "before long", "before the deadline"]),
        ("after", "在…之后", ["after school", "after the meeting", "after lunch", "after graduation", "after a while"]),
    ]

    for prep, meaning, phrases in time_preps:
        for i, phrase in enumerate(phrases):
            # 选择题变体
            distractor_pool = [p for p, _, _ in time_preps if p != prep]
            distractors = random.sample(distractor_pool, min(3, len(distractor_pool)))
            options = [prep] + distractors
            random.shuffle(options)
            answer_idx = options.index(prep)

            q_text = f"The meeting will start ___ {phrase.split(' ', 1)[1] if ' ' in phrase else phrase}."
            # 简化：直接用短语
            q_text = f"Complete: ___ {phrase.split(' ', 1)[1] if ' ' in phrase else phrase}."

            questions.append(make_mc(
                q=f"She arrived ___ {phrase.split(' ', 1)[1] if ' ' in phrase else phrase}.",
                hint=f'表示"{meaning}"',
                options=options,
                answer_idx=answer_idx,
                explain=f'"{prep}" 用于{meaning}。例如：{phrase}。',
                examples=[f'She arrived <strong>{prep}</strong> {phrase.split(" ", 1)[1] if " " in phrase else phrase}.'],
                mnemonic=f'{prep} = {meaning}',
                cat='preposition', diff='easy' if prep in ('at','on','in') else 'medium'
            ))

    # --- 空间介词 ---
    space_preps = [
        ("on", "在…上面（接触表面）", ["the table", "the floor", "the wall", "the ceiling", "the shelf", "the desk", "the ground", "the roof"]),
        ("in", "在…里面", ["the room", "the box", "the car", "the bag", "the drawer", "the water", "the sky", "the garden"]),
        ("at", "在…地点/位置", ["the door", "the bus stop", "the corner", "the top", "the bottom", "the entrance", "the station", "home"]),
        ("under", "在…下面", ["the table", "the bed", "the bridge", "the tree", "the chair", "the umbrella", "the water", "the ground"]),
        ("over", "在…正上方/越过", ["the bridge", "the fence", "the wall", "the mountain", "the rainbow", "the city", "the sea", "the head"]),
        ("between", "在两者之间", ["A and B", "the two buildings", "the lines", "us", "you and me", "the pages"]),
        ("among", "在三者及以上之间", ["the crowd", "the students", "the trees", "the books", "the choices", "friends"]),
        ("behind", "在…后面", ["the door", "the house", "the curtain", "the tree", "the building", "the scenes"]),
        ("in front of", "在…前面", ["the house", "the mirror", "the class", "the building", "the camera", "everyone"]),
        ("next to", "在…旁边", ["the window", "the door", "the bank", "me", "the park", "the library"]),
        ("near", "在…附近", ["the station", "the school", "the river", "the airport", "the market", "here"]),
        ("across", "横穿/在对面", ["the street", "the river", "the bridge", "the road", "the field", "the border"]),
        ("through", "穿过内部", ["the tunnel", "the forest", "the window", "the crowd", "the door", "the park"]),
        ("along", "沿着", ["the road", "the river", "the street", "the path", "the coast", "the corridor"]),
        ("towards", "朝…方向", ["the door", "the city", "the light", "the end", "the goal", "me"]),
        ("around", "围绕/在…周围", ["the corner", "the world", "the house", "the park", "the table", "the city"]),
        ("above", "在…上方（不接触）", ["the clouds", "the horizon", "the line", "average", "sea level", "zero"]),
        ("below", "在…下方", ["the surface", "zero", "average", "the line", "the horizon", "sea level"]),
        ("against", "靠着/反对", ["the wall", "the law", "the rule", "the decision", "the plan", "the wind"]),
        ("onto", "到…上面（动作）", ["the table", "the floor", "the bed", "the stage", "the bus", "the platform"]),
    ]

    for prep, meaning, objects in space_preps:
        for obj in objects:
            distractor_pool = [p for p, _, _ in space_preps if p != prep]
            distractors = random.sample(distractor_pool, min(3, len(distractor_pool)))
            options = [prep] + distractors
            random.shuffle(options)
            answer_idx = options.index(prep)

            diff = 'easy' if prep in ('on','in','at','under') else ('hard' if prep in ('onto','towards','among','against') else 'medium')

            questions.append(make_mc(
                q=f"The cat is ___ {obj}.",
                hint=f'表示"{meaning}"',
                options=options,
                answer_idx=answer_idx,
                explain=f'"{prep}" 表示{meaning}。The cat is {prep} {obj} = 猫在{obj}{meaning.split("…")[-1] if "…" in meaning else meaning}。',
                examples=[f'The cat is <strong>{prep}</strong> {obj}.', f'He stood <strong>{prep}</strong> {obj}.'],
                mnemonic=f'{prep} = {meaning}',
                cat='preposition', diff=diff
            ))

    # --- 动词+介词搭配 ---
    verb_prep_pairs = [
        ("depend on", "依赖/取决于", ["his parents", "the weather", "your decision", "circumstances", "technology", "luck"]),
        ("believe in", "相信/信仰", ["God", "yourself", "hard work", "ghosts", "democracy", "miracles"]),
        ("apply for", "申请", ["a job", "a visa", "a scholarship", "a loan", "a permit", "a position"]),
        ("care about", "关心/在乎", ["the environment", "others", "money", "his health", "fashion", "grades"]),
        ("agree with", "同意（某人/观点）", ["you", "the decision", "his opinion", "the plan", "her suggestion", "the policy"]),
        ("agree on", "就…达成一致", ["the date", "the price", "a plan", "the terms", "the solution", "the schedule"]),
        ("apologize for", "为…道歉", ["the mistake", "being late", "the delay", "the inconvenience", "his behavior", "the error"]),
        ("argue about", "争论", ["money", "politics", "the decision", "trivial matters", "the schedule", "who is right"]),
        ("arrive at", "到达（小地方）", ["the station", "the airport", "school", "the hotel", "the office", "the park"]),
        ("arrive in", "到达（大地方）", ["Beijing", "China", "Europe", "the city", "London", "the country"]),
        ("ask for", "请求/要求", ["help", "advice", "a favor", "directions", "permission", "more time"]),
        ("belong to", "属于", ["me", "the club", "this category", "the museum", "a wealthy family", "the team"]),
        ("complain about", "抱怨", ["the service", "the noise", "the weather", "his boss", "the food", "the price"]),
        ("consist of", "由…组成", ["five parts", "three chapters", "many elements", "several sections", "different colors", "two teams"]),
        ("deal with", "处理/应对", ["the problem", "customers", "difficult situations", "stress", "complaints", "emergencies"]),
        ("dream of/about", "梦想/梦见", ["becoming a doctor", "traveling the world", "flying", "a better future", "fame", "success"]),
        ("insist on", "坚持", ["paying cash", "his innocence", "coming along", "the original plan", "doing it himself", "being right"]),
        ("laugh at", "嘲笑", ["his joke", "the funny movie", "the clown", "her mistake", "the situation", "himself"]),
        ("listen to", "听", ["music", "the radio", "her advice", "the teacher", "the lecture", "his heart"]),
        ("look at", "看", ["the picture", "the sky", "the board", "the map", "the screen", "me"]),
        ("look for", "寻找", ["my keys", "a job", "a solution", "the exit", "information", "help"]),
        ("look after", "照顾", ["the children", "the pets", "the garden", "her mother", "the house", "the patients"]),
        ("look forward to", "期待", ["the holiday", "meeting you", "the concert", "your reply", "the weekend", "graduation"]),
        ("pay for", "支付/为…付出代价", ["the meal", "the damage", "the ticket", "the mistake", "his crime", "the service"]),
        ("rely on", "依赖", ["his parents", "technology", "public transport", "your support", "the data", "her advice"]),
        ("result in", "导致（结果）", ["failure", "success", "confusion", "a disaster", "improvement", "problems"]),
        ("result from", "由…引起", ["carelessness", "poor planning", "the accident", "a misunderstanding", "overwork", "neglect"]),
        ("search for", "搜索/寻找", ["answers", "the truth", "a cure", "information", "meaning", "a solution"]),
        ("suffer from", "遭受/患有", ["a headache", "stress", "poverty", "depression", "a disease", "insomnia"]),
        ("talk about", "谈论", ["the weather", "politics", "their plans", "the movie", "his experience", "the problem"]),
        ("think about", "考虑", ["the offer", "changing jobs", "the future", "the consequences", "her suggestion", "moving abroad"]),
        ("think of", "想到/想起", ["a solution", "a good idea", "his name", "her face", "the past", "a way out"]),
        ("wait for", "等待", ["the bus", "a reply", "the result", "her call", "the signal", "an opportunity"]),
        ("worry about", "担心", ["the exam", "money", "his health", "the future", "being late", "the children"]),
        ("write about", "写关于", ["her travels", "the war", "his childhood", "the environment", "social issues", "science"]),
    ]

    for vp, meaning, objects in verb_prep_pairs:
        for obj in objects:
            # 提取核心介词
            words = vp.split()
            main_prep = words[-1]
            verb = words[0]

            # 构建选项：取其他动词-介词搭配的核心介词作为干扰项
            distractor_preps = [p.split()[-1] for p, _, _ in verb_prep_pairs if p.split()[-1] != main_prep]
            distractor_preps = list(set(distractor_preps))
            distractors = random.sample(distractor_preps, min(3, len(distractor_preps)))
            options_list = [main_prep] + distractors
            random.shuffle(options_list)
            answer_idx = options_list.index(main_prep)

            q_text = f"He {verb} ___ {obj}."
            # 填空题变体
            if random.random() < 0.3:
                questions.append(make_fill(
                    q=f"He {verb} ___ {obj}. ({meaning})",
                    hint=f'"{meaning}"的介词搭配',
                    answer=main_prep,
                    explain=f'"{vp}" = {meaning}。{verb} 后接介词 {main_prep}。',
                    examples=[f'He <strong>{vp}</strong> {obj}.', f'She always <strong>{vp}</strong> {obj}.'],
                    mnemonic=f'{verb} + {main_prep} = {meaning}',
                    cat='preposition', diff='medium'
                ))
            else:
                questions.append(make_mc(
                    q=q_text,
                    hint=f'"{meaning}"',
                    options=options_list,
                    answer_idx=answer_idx,
                    explain=f'"{vp}" = {meaning}。{verb} 后接介词 {main_prep}。',
                    examples=[f'He <strong>{vp}</strong> {obj}.', f'She always <strong>{vp}</strong> {obj}.'],
                    mnemonic=f'{verb} + {main_prep} = {meaning}',
                    cat='preposition', diff='medium'
                ))

    # --- 形容词+介词搭配 ---
    adj_prep_pairs = [
        ("afraid of", "害怕", ["spiders", "the dark", "failure", "heights", "snakes", "flying"]),
        ("angry with/at", "对…生气", ["him", "the situation", "the government", "the delay", "her behavior", "the result"]),
        ("anxious about", "对…焦虑", ["the exam", "his health", "the future", "the interview", "money", "the result"]),
        ("aware of", "意识到", ["the danger", "the problem", "the situation", "his mistake", "the consequences", "the risk"]),
        ("bad at", "不擅长", ["math", "sports", "cooking", "singing", "remembering names", "public speaking"]),
        ("bored with", "对…厌倦", ["the job", "the routine", "the class", "the same food", "waiting", "the movie"]),
        ("capable of", "有能力做", ["great things", "solving it", "improvement", "winning", "leadership", "kindness"]),
        ("confident of/about", "对…有信心", ["success", "the result", "winning", "his ability", "the future", "the plan"]),
        ("crowded with", "挤满了", ["people", "tourists", "cars", "fans", "passengers", "shoppers"]),
        ("different from", "与…不同", ["the others", "what I expected", "his brother", "the original", "the rest", "before"]),
        ("disappointed with", "对…失望", ["the result", "his performance", "the service", "the movie", "the decision", "her behavior"]),
        ("excited about", "对…兴奋", ["the trip", "the news", "the concert", "the game", "the opportunity", "meeting her"]),
        ("famous for", "因…著名", ["its food", "the scenery", "his paintings", "her singing", "the invention", "the architecture"]),
        ("fed up with", "受够了", ["the noise", "his lies", "waiting", "the traffic", "this weather", "the excuses"]),
        ("fond of", "喜欢", ["music", "reading", "animals", "traveling", "sports", "cooking"]),
        ("full of", "充满", ["energy", "ideas", "surprises", "hope", "mistakes", "people"]),
        ("good at", "擅长", ["math", "sports", "drawing", "solving problems", "languages", "cooking"]),
        ("interested in", "对…感兴趣", ["art", "science", "history", "music", "sports", "photography"]),
        ("jealous of", "嫉妒", ["his success", "her beauty", "their wealth", "his talent", "the attention", "her popularity"]),
        ("keen on", "热衷于", ["football", "reading", "traveling", "cooking", "photography", "learning"]),
        ("married to", "与…结婚", ["a doctor", "an artist", "his childhood friend", "a foreigner", "her colleague", "a teacher"]),
        ("nervous about", "对…紧张", ["the exam", "the interview", "the speech", "the result", "meeting her", "flying"]),
        ("opposed to", "反对", ["the plan", "the idea", "the change", "the proposal", "violence", "the new law"]),
        ("pleased with", "对…满意", ["the result", "his work", "the service", "the gift", "her progress", "the outcome"]),
        ("popular with", "受…欢迎", ["students", "young people", "tourists", "the audience", "readers", "voters"]),
        ("proud of", "为…骄傲", ["his son", "her achievement", "their team", "the result", "his work", "being Chinese"]),
        ("responsible for", "对…负责", ["the project", "the accident", "the children", "the mistake", "the team", "the outcome"]),
        ("scared of", "害怕", ["the dark", "spiders", "heights", "ghosts", "public speaking", "flying"]),
        ("similar to", "与…相似", ["the original", "his father", "the previous one", "what I saw", "the example", "each other"]),
        ("sorry for/about", "为…抱歉", ["the mistake", "being late", "the inconvenience", "his loss", "the trouble", "what happened"]),
        ("sure of/about", "确信", ["the answer", "his innocence", "the result", "the direction", "her decision", "the facts"]),
        ("surprised at/by", "对…惊讶", ["the news", "his reaction", "the result", "her behavior", "the change", "the price"]),
        ("tired of", "厌倦了", ["waiting", "the same routine", "his excuses", "this job", "the noise", "studying"]),
        ("used to", "习惯于", ["the weather", "the noise", "early mornings", "the lifestyle", "the food", "working hard"]),
        ("worried about", "担心", ["the exam", "money", "his health", "the future", "being late", "the children"]),
    ]

    for ap, meaning, objects in adj_prep_pairs:
        for obj in objects:
            words = ap.split()
            main_prep = words[-1]
            adj = words[0]

            distractor_preps = [p.split()[-1] for p, _, _ in adj_prep_pairs if p.split()[-1] != main_prep]
            distractor_preps = list(set(distractor_preps))
            distractors = random.sample(distractor_preps, min(3, len(distractor_preps)))
            options_list = [main_prep] + distractors
            random.shuffle(options_list)
            answer_idx = options_list.index(main_prep)

            q_text = f"She is {adj} ___ {obj}."

            if random.random() < 0.3:
                questions.append(make_fill(
                    q=f"She is {adj} ___ {obj}. ({meaning})",
                    hint=f'"{meaning}"的介词搭配',
                    answer=main_prep,
                    explain=f'"{ap}" = {meaning}。形容词 {adj} 后接介词 {main_prep}。',
                    examples=[f'She is <strong>{ap}</strong> {obj}.', f'He is <strong>{ap}</strong> {obj} too.'],
                    mnemonic=f'{adj} + {main_prep} = {meaning}',
                    cat='preposition', diff='medium'
                ))
            else:
                questions.append(make_mc(
                    q=q_text,
                    hint=f'"{meaning}"',
                    options=options_list,
                    answer_idx=answer_idx,
                    explain=f'"{ap}" = {meaning}。形容词 {adj} 后接介词 {main_prep}。',
                    examples=[f'She is <strong>{ap}</strong> {obj}.', f'He is <strong>{ap}</strong> {obj} too.'],
                    mnemonic=f'{adj} + {main_prep} = {meaning}',
                    cat='preposition', diff='medium'
                ))

    # 补充更多介词填空
    fill_preps = [
        ("The book is ___ the table.", "on", "表示在…上面（接触表面）"),
        ("She lives ___ Beijing.", "in", "表示在大城市/国家"),
        ("He arrived ___ the airport.", "at", "表示到达具体地点"),
        ("The cat is hiding ___ the bed.", "under", "表示在…下面"),
        ("We walked ___ the park.", "through", "表示穿过内部"),
        ("She walked ___ the street.", "across", "表示横穿"),
        ("The plane flew ___ the clouds.", "above", "表示在…上方（不接触）"),
        ("He is sitting ___ me and Tom.", "between", "在两者之间"),
        ("She stood ___ the crowd.", "among", "在三者及以上之中"),
        ("The temperature dropped ___ zero.", "below", "在…下方"),
        ("I'll meet you ___ the corner.", "at", "在角落/拐角处"),
        ("He threw the ball ___ the fence.", "over", "越过"),
        ("She looked ___ the window.", "through", "透过"),
        ("We walked ___ the river for hours.", "along", "沿着"),
        ("He leaned ___ the wall.", "against", "靠着"),
        ("She is coming ___ the room.", "into", "进入（动作）"),
        ("He walked ___ of the room.", "out", "走出"),
        ("The restaurant is ___ the corner of Main Street.", "on", "在街角"),
        ("There is a bridge ___ the river.", "over", "在河上方（跨越）"),
        ("She divided the cake ___ three children.", "among", "在多人之间分配"),
        ("The secret is ___ you and me.", "between", "在两者之间（秘密）"),
        ("He has been absent ___ school.", "from", "缺席"),
        ("She is different ___ her sister.", "from", "与…不同"),
        ("I'm tired ___ doing the same thing.", "of", "厌倦"),
        ("He is famous ___ his novels.", "for", "因…著名"),
        ("She is interested ___ learning Japanese.", "in", "对…感兴趣"),
        ("The room is full ___ people.", "of", "充满"),
        ("He is afraid ___ the dark.", "of", "害怕"),
        ("She is good ___ playing piano.", "at", "擅长"),
        ("I'm proud ___ my daughter.", "of", "为…骄傲"),
        ("He is responsible ___ the project.", "for", "负责"),
        ("She is married ___ a lawyer.", "to", "与…结婚"),
        ("They are similar ___ each other.", "to", "与…相似"),
        ("I'm surprised ___ the news.", "at", "对…惊讶"),
        ("He is used ___ hard work.", "to", "习惯于"),
        ("She is worried ___ her son.", "about", "担心"),
        ("I agree ___ you completely.", "with", "同意某人"),
        ("They agreed ___ the date.", "on", "就…达成一致"),
        ("She apologized ___ being late.", "for", "为…道歉"),
        ("He applied ___ the job.", "for", "申请"),
        ("I believe ___ hard work.", "in", "相信"),
        ("She belongs ___ this club.", "to", "属于"),
        ("They complained ___ the noise.", "about", "抱怨"),
        ("The team consists ___ five members.", "of", "由…组成"),
        ("We should deal ___ this problem now.", "with", "处理"),
        ("I dream ___ traveling the world.", "of", "梦想"),
        ("He insisted ___ paying the bill.", "on", "坚持"),
        ("Don't laugh ___ other people.", "at", "嘲笑"),
        ("Please listen ___ what I'm saying.", "to", "听"),
        ("I'm looking ___ my keys.", "for", "寻找"),
        ("Can you look ___ my cat this weekend?", "after", "照顾"),
        ("I look forward ___ meeting you.", "to", "期待"),
        ("Who will pay ___ the meal?", "for", "支付"),
        ("I rely ___ public transport.", "on", "依赖"),
        ("Hard work results ___ success.", "in", "导致（结果）"),
        ("Success results ___ hard work.", "from", "由…引起"),
        ("She suffers ___ headaches.", "from", "患有/遭受"),
        ("Let's talk ___ your plans.", "about", "谈论"),
        ("I'm thinking ___ changing my job.", "about", "考虑"),
        ("What do you think ___ this idea?", "of", "认为/想到"),
        ("We're waiting ___ the bus.", "for", "等待"),
        ("Don't worry ___ the exam.", "about", "担心"),
        ("He arrived ___ time for the meeting.", "on", "准时"),
        ("She arrived ___ time to catch the train.", "in", "及时（赶上）"),
        ("He is ___ work now.", "at", "在工作"),
        ("She is ___ a meeting.", "in", "在开会"),
        ("I saw it ___ TV.", "on", "在电视上"),
        ("He is ___ the phone.", "on", "在打电话"),
        ("She went to school ___ foot.", "on", "步行"),
        ("He goes to work ___ car.", "by", "乘交通工具"),
        ("She wrote the letter ___ hand.", "by", "用手写"),
        ("They paid ___ cash.", "in", "用现金"),
        ("I paid ___ credit card.", "by", "用信用卡"),
        ("He did it ___ purpose.", "on", "故意"),
        ("She did it ___ accident.", "by", "偶然/意外"),
        ("He is ___ a hurry.", "in", "匆忙"),
        ("She is ___ trouble.", "in", "遇到麻烦"),
        ("He is ___ danger.", "in", "处于危险中"),
        ("The house is ___ fire!", "on", "着火"),
        ("She is ___ holiday.", "on", "在度假"),
        ("He is ___ business.", "on", "出差"),
        ("I'm ___ a diet.", "on", "在节食"),
        ("She is ___ the way home.", "on", "在回家的路上"),
        ("He is ___ his way to success.", "on", "在通往成功的路上"),
        ("___ the whole, it's a good plan.", "On", "总的来说"),
        ("___ average, he reads two books a month.", "On", "平均"),
        ("___ my opinion, this is wrong.", "In", "在我看来"),
        ("___ conclusion, we agree.", "In", "总之"),
        ("___ addition, there is another problem.", "In", "此外"),
        ("___ fact, I don't like it.", "In", "事实上"),
        ("___ the end, they won.", "In", "最终（结果）"),
        ("___ the beginning of the movie, ...", "At", "在…开始时"),
        ("___ first, I didn't understand.", "At", "起初"),
        ("___ last, he arrived.", "At", "终于"),
        ("___ present, she is studying.", "At", "目前"),
        ("___ the same time, ...", "At", "同时"),
        ("___ once, he understood.", "At", "立刻/马上"),
        ("She is ___ school now.", "at", "在学校（上课）"),
        ("He is ___ hospital.", "in", "在医院（住院）"),
        ("She is ___ bed.", "in", "在床上（睡觉）"),
    ]

    for q, ans, meaning in fill_preps:
        questions.append(make_fill(
            q=q, hint=f'表示"{meaning}"',
            answer=ans,
            explain=f'"{ans}" 用于{meaning}。',
            examples=[q.replace("___", f"<strong>{ans}</strong>")],
            mnemonic=f'{ans} = {meaning}',
            cat='preposition',
            diff='easy' if ans in ('on','in','at','under') else 'medium'
        ))

    return questions


# ============================================================
# 2. ARTICLES 冠词题库 (目标 400 题)
# ============================================================
def generate_articles():
    questions = []

    # 不定冠词 a/an
    a_words = ["book", "car", "dog", "house", "pen", "table", "chair", "phone", "computer", "university",
               "European country", "one-way street", "useful tool", "uniform", "unit", "unicorn", "user"]
    an_words = ["apple", "elephant", "hour", "honest man", "umbrella", "orange", "egg", "island", "engineer",
                "MBA", "MP3", "X-ray", "honor", "heir", "uncle", "aunt", "idea", "interesting book"]

    for w in a_words:
        questions.append(make_mc(
            q=f"I need ___ {w}.",
            hint=f'{w} 以辅音音素开头',
            options=['a', 'an', 'the', '—(无)'],
            answer_idx=0,
            explain=f'"{w}" 以辅音音素开头，用 a。注意：university 发音以 /j/（辅音）开头，用 a 不是 an。',
            examples=[f'I need <strong>a</strong> {w}.'],
            mnemonic=f'{w} 发音开头是辅音 → a',
            cat='article', diff='easy'
        ))
    for w in an_words:
        questions.append(make_mc(
            q=f"She is ___ {w}.",
            hint=f'{w} 以元音音素开头',
            options=['a', 'an', 'the', '—(无)'],
            answer_idx=1,
            explain=f'"{w}" 以元音音素开头，用 an。注意：hour 的 h 不发音，以元音 /aʊ/ 开头，用 an。',
            examples=[f'She is <strong>an</strong> {w}.'],
            mnemonic=f'{w} 发音开头是元音 → an',
            cat='article', diff='easy'
        ))

    # 定冠词 the
    the_scenarios = [
        ("特指双方都知道的", ["Please close ___ door.", "Did you feed ___ cat?", "Turn off ___ TV, please.",
                           "___ phone is ringing.", "Where is ___ remote?", "Open ___ window."]),
        ("独一无二的事物", ["___ sun rises in the east.", "___ moon is bright tonight.", "___ earth goes around the sun.",
                         "___ sky is clear.", "___ world is changing.", "___ universe is vast."]),
        ("序数词前", ["He was ___ first to arrive.", "This is ___ second time.", "She won ___ third prize.",
                    "___ first impression matters.", "He lives on ___ fifth floor."]),
        ("形容词最高级前", ["She is ___ tallest in class.", "This is ___ best option.", "He is ___ most intelligent person I know.",
                         "It was ___ worst day ever.", "This is ___ easiest way."]),
        ("乐器前", ["She plays ___ piano.", "He plays ___ violin.", "I'm learning ___ guitar.",
                  "She plays ___ flute.", "He plays ___ drums."]),
        ("江河湖海山脉", ["___ Yangtze River is long.", "___ Pacific is vast.", "___ Alps are beautiful.",
                        "___ Nile flows north.", "___ Himalayas are in Asia."]),
        ("the + adj 表示一类人", ["___ rich should help ___ poor.", "___ young should respect ___ old.",
                               "___ injured were taken to hospital.", "___ blind need special facilities.",
                               "___ homeless need our help.", "___ disabled face many challenges."]),
    ]

    for scenario, sentences in the_scenarios:
        for s in sentences:
            questions.append(make_fill(
                q=s, hint=f'{scenario}',
                answer='the',
                explain=f'此处用 the，因为{scenario}。',
                examples=[s.replace("___", "<strong>The</strong>")],
                mnemonic=f'{scenario} → the',
                cat='article', diff='easy' if '特指' in scenario or '独一无二' in scenario else 'medium'
            ))

    # 不用冠词的情况
    no_article = [
        ("三餐前", ["We have lunch at noon.", "Breakfast is ready.", "What's for dinner?",
                   "She had supper at 7.", "Let's have breakfast together."]),
        ("球类运动前", ["He plays basketball.", "She likes football.", "I play tennis on weekends.",
                      "They play volleyball.", "We enjoy badminton."]),
        ("星期/月份/季节", ["Monday is a busy day.", "January is cold.", "Summer is my favorite season.",
                         "We met on Friday.", "Spring has arrived."]),
        ("学科名称", ["Math is difficult.", "I love history.", "She studies chemistry.",
                    "Physics is interesting.", "He teaches English."]),
        ("节日（多数）", ["Christmas is coming.", "New Year is around the corner.", "Easter is in spring.",
                        "Halloween is fun."]),
        ("称呼/头衔+名字", ["President Obama spoke.", "Doctor Smith is here.", "Professor Li will lecture.",
                          "Captain America is a hero."]),
        ("by + 交通工具", ["by bus", "by train", "by plane", "by car", "by bike", "by boat"]),
    ]

    for scenario, sentences in no_article:
        for s in sentences:
            # 在句中插入空位
            words = s.split()
            # 找名词位置插入冠词选择
            if scenario == "by + 交通工具":
                questions.append(make_mc(
                    q=f"I go to work ___.",
                    hint=f'{scenario}不用冠词',
                    options=[s, f"by the {s.split()[-1]}", f"by a {s.split()[-1]}", f"on {s.split()[-1]}"],
                    answer_idx=0,
                    explain=f'{scenario}时不加冠词。{s}（不加 the/a）。',
                    examples=[f'I go to work <strong>{s}</strong>.'],
                    mnemonic=f'{scenario} → 无冠词',
                    cat='article', diff='easy'
                ))
            else:
                questions.append(make_mc(
                    q=f"Complete: ___, {s.lower()}",
                    hint=f'{scenario}不用冠词',
                    options=['—(无冠词)', 'The', 'A', 'An'],
                    answer_idx=0,
                    explain=f'{scenario}时通常不加冠词。',
                    examples=[f'<strong>{s}</strong>'],
                    mnemonic=f'{scenario} → 无冠词',
                    cat='article', diff='easy'
                ))

    # 更多冠词综合题
    comprehensive = [
        ("I saw ___ movie last night. ___ movie was great.", ["a / The", "the / A", "an / The", "— / —"], 0,
         "第一次提及用 a，再次提及用 the。", "首次 → a/an；再次 → the"),
        ("She is ___ university student.", ["a", "an", "the", "—"], 0,
         "university 发音以 /j/（辅音）开头，用 a。", "university → a（/juː/ 辅音开头）"),
        ("He is ___ honest person.", ["a", "an", "the", "—"], 1,
         "honest 的 h 不发音，以元音 /ɒ/ 开头，用 an。", "honest → an（h 不发音）"),
        ("___ Great Wall is a wonder of the world.", ["The", "A", "An", "—"], 0,
         "专有名词（长城）前用 the。", "专有名词（长城/故宫/颐和园）→ the"),
        ("She goes to ___ school by bus.", ["the", "a", "an", "—"], 3,
         "go to school 表示去上学（功能目的），不加冠词。", "上学 → go to school（无冠词）"),
        ("He is in ___ hospital with a broken leg.", ["the", "a", "an", "—"], 3,
         "in hospital（英式）表示住院，不加冠词。", "住院 → in hospital（无冠词，英式）"),
        ("___ more you practice, ___ better you get.", ["The / the", "A / a", "— / —", "The / a"], 0,
         "\"the more...the more...\" 固定结构。", "the more...the more... = 越...越..."),
        ("I need ___ information about this.", ["an", "a", "the", "—"], 3,
         "information 是不可数名词，不用 a/an。此处非特指，不用 the。", "不可数名词（泛指）→ 无冠词"),
        ("___ Chinese are known for their cuisine.", ["The", "A", "An", "—"], 0,
         "the + 国籍形容词 表示这个国家的人（全体）。", "the Chinese/Japanese/French = ...国人"),
        ("He has ___ great deal of work to do.", ["a", "an", "the", "—"], 0,
         "a great deal of = 大量（固定搭配，不可数名词的量词）。", "a great deal of = 大量（固定搭配）"),
    ]

    for q, options, ans, explain, mnemonic in comprehensive:
        questions.append(make_mc(
            q=q, hint='选择正确的冠词',
            options=options, answer_idx=ans,
            explain=explain,
            examples=[q.replace("___", f"<strong>{options[ans]}</strong>")],
            mnemonic=mnemonic,
            cat='article', diff='medium'
        ))

    return questions


# ============================================================
# 主程序
# ============================================================
def main():
    print("开始生成题库...")

    generators = [
        ("prepositions.json", generate_prepositions, 500),
        ("articles.json", generate_articles, 400),
    ]

    total = 0
    for filename, gen_func, target in generators:
        questions = gen_func()
        filepath = os.path.join(OUTPUT_DIR, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(questions, f, ensure_ascii=False, indent=2)
        print(f"✅ {filename}: {len(questions)} 题 (目标 {target})")
        total += len(questions)

    print(f"\n🎉 总计生成 {total} 题")

if __name__ == "__main__":
    main()
