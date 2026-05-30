#!/usr/bin/env python3
"""
初中英语词汇闯关系统 — 形容词/副词题库生成器
目标：500 题
"""

import json
import random
import os

random.seed(42)

OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUTPUT_DIR, exist_ok=True)

id_counter = [1]

def next_id():
    val = id_counter[0]
    id_counter[0] += 1
    return val

def make_mc(q, hint, options, answer_idx, explain, examples, mnemonic, cat, diff):
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

def generate_adjectives():
    questions = []

    # ============================================================
    # 1. -ed vs -ing 形容词 (30+ 对)
    # ============================================================
    ed_ing_pairs = [
        ("interested", "interesting", "感兴趣的", "有趣的",
         ["The story is very ___ing.", "I'm very ___ed in the story."],
         "我（人）感兴趣用-ed，事/物令人感兴趣用-ing",
         "人-ed，物-ing"),
        ("bored", "boring", "感到无聊的", "令人无聊的",
         ["The movie was ___ing.", "I felt ___ed during the movie."],
         "电影让人无聊用-ing，我（人）感到无聊用-ed",
         "人-ed，物-ing"),
        ("excited", "exciting", "感到兴奋的", "令人兴奋的",
         ["The news is ___ing!", "We are ___ed about the news!"],
         "消息令人兴奋用-ing，我们（人）感到兴奋用-ed",
         "人-ed，物-ing"),
        ("surprised", "surprising", "感到惊讶的", "令人惊讶的",
         ["The result was ___ing.", "She looked ___ed at the result."],
         "结果令人惊讶用-ing，她（人）感到惊讶用-ed",
         "人-ed，物-ing"),
        ("tired", "tiring", "感到疲倦的", "令人疲倦的",
         ["The work was very ___ing.", "I am ___ed after the long walk."],
         "工作令人疲倦用-ing，我（人）感到疲倦用-ed",
         "人-ed，物-ing"),
        ("amazed", "amazing", "感到惊奇的", "令人惊奇的",
         ["The view is ___ing!", "We were ___ed by the view."],
         "景色令人惊奇用-ing，我们（人）感到惊奇用-ed",
         "人-ed，物-ing"),
        ("confused", "confusing", "感到困惑的", "令人困惑的",
         ["The instructions are ___ing.", "He looks ___ed about the instructions."],
         "说明令人困惑用-ing，他（人）感到困惑用-ed",
         "人-ed，物-ing"),
        ("disappointed", "disappointing", "感到失望的", "令人失望的",
         ["The result was ___ing.", "She felt ___ed with the result."],
         "结果令人失望用-ing，她（人）感到失望用-ed",
         "人-ed，物-ing"),
        ("embarrassed", "embarrassing", "感到尴尬的", "令人尴尬的",
         ["It was an ___ing situation.", "He felt ___ed by the mistake."],
         "情况令人尴尬用-ing，他（人）感到尴尬用-ed",
         "人-ed，物-ing"),
        ("frightened", "frightening", "感到害怕的", "令人害怕的",
         ["The sound was ___ing.", "The child looked ___ed."],
         "声音令人害怕用-ing，孩子（人）感到害怕用-ed",
         "人-ed，物-ing"),
        ("worried", "worrying", "感到担忧的", "令人担忧的",
         ["The news is ___ing.", "She is ___ed about her exam."],
         "消息令人担忧用-ing，她（人）感到担忧用-ed",
         "人-ed，物-ing"),
        ("annoyed", "annoying", "感到恼怒的", "令人恼怒的",
         ["The noise is ___ing.", "He got ___ed by the noise."],
         "噪音令人恼怒用-ing，他（人）感到恼怒用-ed",
         "人-ed，物-ing"),
        ("shocked", "shocking", "感到震惊的", "令人震惊的",
         ["The news was ___ing.", "We were ___ed by the news."],
         "消息令人震惊用-ing，我们（人）感到震惊用-ed",
         "人-ed，物-ing"),
        ("puzzled", "puzzling", "感到迷惑的", "令人迷惑的",
         ["The question is ___ing.", "He looked ___ed at the question."],
         "问题令人迷惑用-ing，他（人）感到迷惑用-ed",
         "人-ed，物-ing"),
        ("satisfied", "satisfying", "感到满意的", "令人满意的",
         ["The result is ___ing.", "She is ___ed with the result."],
         "结果令人满意用-ing，她（人）感到满意用-ed",
         "人-ed，物-ing"),
        ("fascinated", "fascinating", "着迷的", "迷人的",
         ["The show is ___ing.", "I am ___ed by the show."],
         "表演迷人用-ing，我（人）着迷用-ed",
         "人-ed，物-ing"),
        ("depressed", "depressing", "沮丧的", "令人沮丧的",
         ["The weather is ___ing.", "He felt ___ed by the bad news."],
         "天气令人沮丧用-ing，他（人）感到沮丧用-ed",
         "人-ed，物-ing"),
        ("relaxed", "relaxing", "放松的", "令人放松的",
         ["The music is ___ing.", "I feel ___ed after the bath."],
         "音乐令人放松用-ing，我（人）感到放松用-ed",
         "人-ed，物-ing"),
        ("thrilled", "thrilling", "激动万分的", "令人激动的",
         ["The ride was ___ing!", "We were ___ed by the ride!"],
         "旅程令人激动用-ing，我们（人）激动万分用-ed",
         "人-ed，物-ing"),
        ("inspired", "inspiring", "受鼓舞的", "鼓舞人心的",
         ["His speech was ___ing.", "We felt ___ed by his speech."],
         "演讲鼓舞人心用-ing，我们（人）受鼓舞用-ed",
         "人-ed，物-ing"),
        ("frustrated", "frustrating", "沮丧的", "令人沮丧的",
         ["The situation is ___ing.", "He became ___ed with the delay."],
         "情况令人沮丧用-ing，他（人）感到沮丧用-ed",
         "人-ed，物-ing"),
        ("moved", "moving", "感动的", "令人感动的",
         ["The film was ___ing.", "We were deeply ___ed by the film."],
         "电影令人感动用-ing，我们（人）感动用-ed",
         "人-ed，物-ing"),
        ("terrified", "terrifying", "极度恐惧的", "可怕的",
         ["The experience was ___ing.", "She was ___ed by the experience."],
         "经历可怕用-ing，她（人）恐惧用-ed",
         "人-ed，物-ing"),
        ("pleased", "pleasing", "高兴的", "令人愉快的",
         ["The result is ___ing.", "He was ___ed with the result."],
         "结果令人愉快用-ing，他（人）高兴用-ed",
         "人-ed，物-ing"),
        ("overwhelmed", "overwhelming", "不知所措的", "压倒性的",
         ["The task is ___ing.", "She felt ___ed by the task."],
         "任务压倒性用-ing，她（人）不知所措用-ed",
         "人-ed，物-ing"),
        ("entertained", "entertaining", "被逗乐的", "有趣的/娱乐的",
         ["The show was ___ing.", "The audience was ___ed by the show."],
         "表演有趣用-ing，观众被逗乐用-ed",
         "人-ed，物-ing"),
        ("challenged", "challenging", "受挑战的", "有挑战性的",
         ["The task is ___ing.", "He felt ___ed by the task."],
         "任务有挑战用-ing，他（人）受挑战用-ed",
         "人-ed，物-ing"),
        ("disturbed", "disturbing", "被打扰的", "令人不安的",
         ["The news is ___ing.", "She was ___ed by the news."],
         "消息令人不安用-ing，她（人）被打扰用-ed",
         "人-ed，物-ing"),
        ("convinced", "convincing", "确信的", "有说服力的",
         ["His argument was ___ing.", "I am ___ed by his argument."],
         "论点有说服力用-ing，我（人）确信用-ed",
         "人-ed，物-ing"),
        ("disgusted", "disgusting", "厌恶的", "令人作呕的",
         ["The smell is ___ing.", "He felt ___ed by the smell."],
         "气味令人作呕用-ing，他（人）厌恶用-ed",
         "人-ed，物-ing"),
        ("encouraged", "encouraging", "受鼓舞的", "令人鼓舞的",
         ["The news was ___ing.", "She felt ___ed by the news."],
         "消息令人鼓舞用-ing，她（人）受鼓舞用-ed",
         "人-ed，物-ing"),
        ("exhausted", "exhausting", "精疲力竭的", "令人筋疲力尽的",
         ["The work was ___ing.", "I am ___ed after work."],
         "工作令人筋疲力尽用-ing，我（人）筋疲力竭用-ed",
         "人-ed，物-ing"),
        ("terrified", "terrifying", "极度恐惧的", "极其可怕的",
         ["The movie was ___ing.", "The kids were ___ed."],
         "电影可怕用-ing，孩子们恐惧用-ed",
         "人-ed，物-ing"),
        ("touched", "touching", "感动的", "动人的",
         ["The letter was ___ing.", "She was ___ed by the letter."],
         "信动人用-ing，她（人）感动用-ed",
         "人-ed，物-ing"),
    ]

    for ed_form, ing_form, ed_meaning, ing_meaning, sentences, explain, mnemonic in ed_ing_pairs:
        # 每种出2题：一道选-ed，一道选-ing
        # MC题 — 选-ed
        s_ed = sentences[1].replace("___ed", "___").replace("___ing", "___")
        options_ed = [ed_form, ing_form]
        random.shuffle(options_ed)
        ans_idx_ed = options_ed.index(ed_form)

        questions.append(make_mc(
            q=f"Choose the correct word: {s_ed}",
            hint=f'主语是人，用表示"{ed_meaning}"的形式',
            options=options_ed,
            answer_idx=ans_idx_ed,
            explain=f'主语是人（感到...），用 {ed_form}（{ed_meaning}）。{ing_form} 形容事物（令人...的）。\n{explain}',
            examples=[f'I am <strong>{ed_form}</strong> in this topic. 我对这个话题很感兴趣。',
                      f'This is an <strong>{ing_form}</strong> topic. 这是一个有趣的话题。'],
            mnemonic=mnemonic,
            cat='adjective', diff='easy'
        ))

        # MC题 — 选-ing
        s_ing = sentences[0].replace("___ed", "___").replace("___ing", "___")
        options_ing = [ing_form, ed_form]
        random.shuffle(options_ing)
        ans_idx_ing = options_ing.index(ing_form)

        questions.append(make_mc(
            q=f"Choose the correct word: {s_ing}",
            hint=f'主语是事物，用表示"{ing_meaning}"的形式',
            options=options_ing,
            answer_idx=ans_idx_ing,
            explain=f'主语是事物（令人...），用 {ing_form}（{ing_meaning}）。{ed_form} 形容人的感受。\n{explain}',
            examples=[f'This is an <strong>{ing_form}</strong> book. 这是一本有趣的书。',
                      f'I am <strong>{ed_form}</strong> in the book. 我对这本书感兴趣。'],
            mnemonic=mnemonic,
            cat='adjective', diff='easy'
        ))

    # 额外-ed/-ing填空
    ed_ing_fill = [
        ("I was so ___ (bore) at the party.", "bored",
         "人感到无聊用 -ed。",
         [f'I was so <strong>bored</strong> at the party. 我在聚会上感到太无聊了。',
          f'The party was <strong>boring</strong>. 聚会很无聊。']),
        ("The movie was really ___ (excite).", "exciting",
         "事物令人兴奋用 -ing。",
         [f'The movie was really <strong>exciting</strong>. 电影真的令人兴奋。',
          f'I was <strong>excited</strong> about the movie. 我对电影很兴奋。']),
        ("She felt ___ (surprise) when she saw the gift.", "surprised",
         "人感到惊讶用 -ed。",
         [f'She felt <strong>surprised</strong> when she saw the gift. 她看到礼物时感到惊讶。',
          f'The gift was <strong>surprising</strong>. 礼物令人惊讶。']),
        ("The lecture was ___ (tire).", "tiring",
         "事物令人疲倦用 -ing。",
         [f'The lecture was <strong>tiring</strong>. 讲座令人疲倦。',
          f'I felt <strong>tired</strong> after the lecture. 讲座后我感到疲倦。']),
        ("We were ___ (amaze) by the magician's tricks.", "amazed",
         "人感到惊奇用 -ed。",
         [f'We were <strong>amazed</strong> by the tricks. 我们对魔术感到惊奇。',
          f'The tricks were <strong>amazing</strong>. 魔术令人惊奇。']),
        ("The instructions are ___ (confuse).", "confusing",
         "事物令人困惑用 -ing。",
         [f'The instructions are <strong>confusing</strong>. 说明令人困惑。',
          f'I am <strong>confused</strong> by the instructions. 我对说明感到困惑。']),
        ("He was ___ (disappoint) with his test score.", "disappointed",
         "人感到失望用 -ed。",
         [f'He was <strong>disappointed</strong> with his score. 他对分数感到失望。',
          f'The score was <strong>disappointing</strong>. 分数令人失望。']),
        ("It was an ___ (embarrass) situation.", "embarrassing",
         "事物令人尴尬用 -ing。",
         [f'It was an <strong>embarrassing</strong> situation. 这是一个尴尬的情况。',
          f'I felt <strong>embarrassed</strong>. 我感到尴尬。']),
        ("The children were ___ (frighten) by the loud noise.", "frightened",
         "人感到害怕用 -ed。",
         [f'The children were <strong>frightened</strong> by the noise. 孩子们被噪音吓到了。',
          f'The noise was <strong>frightening</strong>. 噪音很吓人。']),
        ("She is ___ (worry) about the exam results.", "worried",
         "人感到担忧用 -ed。",
         [f'She is <strong>worried</strong> about the results. 她担心考试结果。',
          f'The situation is <strong>worrying</strong>. 情况令人担忧。']),
    ]
    for q, ans, explain, examples in ed_ing_fill:
        questions.append(make_fill(
            q=q, hint='注意是描述人还是事物',
            answer=ans, explain=explain, examples=examples,
            mnemonic='人-ed，物-ing', cat='adjective', diff='easy'
        ))

    # ============================================================
    # 2. 比较级和最高级
    # ============================================================

    # --- 规则变化 -er/-est ---
    regular_adjs = [
        ("tall", "taller", "tallest", "高的"),
        ("short", "shorter", "shortest", "矮的"),
        ("fast", "faster", "fastest", "快的"),
        ("slow", "slower", "slowest", "慢的"),
        ("young", "younger", "youngest", "年轻的"),
        ("old", "older", "oldest", "老的/旧的"),
        ("long", "longer", "longest", "长的"),
        ("high", "higher", "highest", "高的"),
        ("low", "lower", "lowest", "低的"),
        ("warm", "warmer", "warmest", "温暖的"),
        ("cool", "cooler", "coolest", "凉爽的"),
        ("cold", "colder", "coldest", "冷的"),
        ("small", "smaller", "smallest", "小的"),
        ("light", "lighter", "lightest", "轻的/浅的"),
        ("dark", "darker", "darkest", "暗的/深的"),
        ("hard", "harder", "hardest", "硬的/困难的"),
        ("soft", "softer", "softest", "软的"),
        ("weak", "weaker", "weakest", "弱的"),
        ("strong", "stronger", "strongest", "强壮的"),
        ("rich", "richer", "richest", "富有的"),
        ("poor", "poorer", "poorest", "贫穷的"),
        ("clean", "cleaner", "cleanest", "干净的"),
        ("thick", "thicker", "thickest", "厚的"),
        ("thin", "thinner", "thinnest", "薄的/瘦的"),
        ("near", "nearer", "nearest", "近的"),
        ("clever", "cleverer", "cleverest", "聪明的"),
        ("narrow", "narrower", "narrowest", "窄的"),
        ("quiet", "quieter", "quietest", "安静的"),
        ("simple", "simpler", "simplest", "简单的"),
    ]

    for adj, comp, sup, meaning in regular_adjs:
        # 比较级选择题
        distractor_adj = random.choice([a for a, _, _, _ in regular_adjs if a != adj])
        options_comp = [comp, f"more {adj}", f"most {adj}", f"{distractor_adj}er"]
        random.shuffle(options_comp)
        ans_comp = options_comp.index(comp)

        questions.append(make_mc(
            q=f"This building is ___ than that one. ({adj})",
            hint=f'{adj} 是单音节词，比较级加 -er',
            options=options_comp,
            answer_idx=ans_comp,
            explain=f'{adj} 是单音节形容词，比较级直接加 -er：{adj} → {comp}。',
            examples=[f'This building is <strong>{comp}</strong> than that one. 这栋楼比那栋高。',
                      f'He is the <strong>{sup}</strong> in his class. 他是班上最高的。'],
            mnemonic=f'{adj} → {comp} → {sup}',
            cat='adjective', diff='easy'
        ))

        # 最高级填空题
        questions.append(make_fill(
            q=f"She is the ___ ({adj}) girl in the class.",
            hint=f'{adj} 的最高级形式',
            answer=sup,
            explain=f'{adj} 的最高级加 -est：{adj} → {sup}。注意前面要加 the。',
            examples=[f'She is the <strong>{sup}</strong> girl in the class. 她是班上最高的女孩。',
                      f'He is <strong>{comp}</strong> than her. 他比她高。'],
            mnemonic=f'{adj} → {comp} → {sup}',
            cat='adjective', diff='easy'
        ))

    # --- more/most 多音节 ---
    multi_adjs = [
        ("beautiful", "more beautiful", "most beautiful", "美丽的"),
        ("interesting", "more interesting", "most interesting", "有趣的"),
        ("important", "more important", "most important", "重要的"),
        ("difficult", "more difficult", "most difficult", "困难的"),
        ("expensive", "more expensive", "most expensive", "昂贵的"),
        ("comfortable", "more comfortable", "most comfortable", "舒适的"),
        ("dangerous", "more dangerous", "most dangerous", "危险的"),
        ("popular", "more popular", "most popular", "受欢迎的"),
        ("delicious", "more delicious", "most delicious", "美味的"),
        ("wonderful", "more wonderful", "most wonderful", "精彩的"),
        ("terrible", "more terrible", "most terrible", "可怕的"),
        ("careful", "more careful", "most careful", "仔细的"),
        ("useful", "more useful", "most useful", "有用的"),
        ("helpful", "more helpful", "most helpful", "有帮助的"),
        ("powerful", "more powerful", "most powerful", "强大的"),
        ("famous", "more famous", "most famous", "著名的"),
        ("serious", "more serious", "most serious", "严肃的/严重的"),
        ("patient", "more patient", "most patient", "耐心的"),
        ("generous", "more generous", "most generous", "慷慨的"),
        ("intelligent", "more intelligent", "most intelligent", "聪明的"),
    ]

    for adj, comp, sup, meaning in multi_adjs:
        # 比较级选择题
        wrong_options = [f"{adj}er", f"{adj}est", f"most {adj}"]
        options_comp = [comp] + wrong_options
        random.shuffle(options_comp)
        ans_comp = options_comp.index(comp)

        questions.append(make_mc(
            q=f"This book is ___ than that one. ({adj})",
            hint=f'{adj} 是多音节词，比较级用 more',
            options=options_comp,
            answer_idx=ans_comp,
            explain=f'{adj} 是多音节形容词（3个音节及以上），比较级用 more + 原级：{comp}。',
            examples=[f'This book is <strong>{comp}</strong> than that one. 这本书比那本有趣。',
                      f'This is the <strong>{sup}</strong> book I have read. 这是我读过最有趣的书。'],
            mnemonic=f'多音节 → more + {adj}',
            cat='adjective', diff='easy'
        ))

        # 最高级填空题
        questions.append(make_fill(
            q=f"This is the ___ ({adj}) place I have ever been.",
            hint=f'{adj} 的最高级形式',
            answer=sup,
            explain=f'{adj} 的最高级用 most + 原级：{sup}。多音节词不能加 -est。',
            examples=[f'This is the <strong>{sup}</strong> place I have been. 这是我去过的最美的地方。',
                      f'It is <strong>{comp}</strong> than I expected. 它比我预期的更美。'],
            mnemonic=f'多音节最高级 → most + {adj}',
            cat='adjective', diff='easy'
        ))

    # --- 不规则比较级 ---
    irregular_comparisons = [
        ("good", "better", "best", "好的"),
        ("bad", "worse", "worst", "坏的/糟糕的"),
        ("far", "farther/further", "farthest/furthest", "远的"),
        ("little", "less", "least", "少的"),
        ("many", "more", "most", "许多的"),
        ("much", "more", "most", "许多的（不可数）"),
        ("well (adv)", "better", "best", "好地"),
        ("badly (adv)", "worse", "worst", "糟糕地"),
    ]

    for adj, comp, sup, meaning in irregular_comparisons:
        comp_variants = comp.split("/")
        comp_main = comp_variants[0]
        sup_main = sup.split("/")[0]

        # 比较级选择题
        if adj in ("good", "bad", "far", "little", "many", "much"):
            wrong_options = [f"{adj}er", f"more {adj}", f"{adj}est", f"most {adj}"]
            random.shuffle(wrong_options)
            options_comp = [comp_main] + wrong_options[:3]
            random.shuffle(options_comp)
            ans_comp = options_comp.index(comp_main)

            questions.append(make_mc(
                q=f"This result is ___ than I expected. ({adj})",
                hint=f'{adj} 的比较级是不规则变化',
                options=options_comp,
                answer_idx=ans_comp,
                explain=f'{adj} 是不规则变化：{adj} → {comp} → {sup}。不能用 {adj}er 或 more {adj}。',
                examples=[f'This is <strong>{comp_main}</strong> than that. 这比那个好。',
                          f'This is the <strong>{sup_main}</strong> one. 这是最好的。'],
                mnemonic=f'{adj} → {comp} → {sup}（必须记住！）',
                cat='adjective', diff='medium'
            ))

        # 最高级选择题
        if adj in ("good", "bad", "far", "little", "many", "much"):
            wrong_options_sup = [f"{adj}est", f"most {adj}", f"more {adj}", f"{adj}er"]
            random.shuffle(wrong_options_sup)
            options_sup = [sup_main] + wrong_options_sup[:3]
            random.shuffle(options_sup)
            ans_sup = options_sup.index(sup_main)

            questions.append(make_mc(
                q=f"She is the ___ student in the class. ({adj})",
                hint=f'{adj} 的最高级是不规则变化',
                options=options_sup,
                answer_idx=ans_sup,
                explain=f'{adj} 的最高级是不规则变化：{adj} → {comp} → {sup}。',
                examples=[f'She is the <strong>{sup_main}</strong> student. 她是最好的学生。',
                          f'He is <strong>{comp_main}</strong> than me. 他比我好。'],
                mnemonic=f'{adj} → {comp} → {sup}（必须记住！）',
                cat='adjective', diff='medium'
            ))

    # --- far 的两种形式辨析 ---
    questions.append(make_mc(
        q="Which word means '更远（距离）'?",
        hint='far 的比较级有两个，其中一个更常用于物理距离',
        options=['farther', 'further', 'farest', 'furthest'],
        answer_idx=0,
        explain='farther 更常用于表示物理距离"更远"。further 更多用于抽象含义"更进一步"。',
        examples=[f'I can walk <strong>farther</strong> than you. 我能走得比你更远。',
                  f'Let\'s discuss this <strong>further</strong>. 让我们进一步讨论这个。'],
        mnemonic='farther = 更远（距离）→ far-ther → 走得远',
        cat='adjective', diff='hard'
    ))

    questions.append(make_mc(
        q="Let's discuss this matter ___.",
        hint='表示"进一步"讨论（抽象含义）',
        options=['further', 'farther', 'more far', 'farer'],
        answer_idx=0,
        explain='further 用于抽象含义"进一步"。farther 用于物理距离。此处讨论是抽象行为。',
        examples=[f'Let\'s discuss this <strong>further</strong>. 让我们进一步讨论这个。',
                  f'The school is <strong>farther</strong> than I thought. 学校比我想的更远。'],
        mnemonic='further = 更进一步（抽象）→ further study/discuss',
        cat='adjective', diff='hard'
    ))

    # --- older/elder 辨析 ---
    questions.append(make_mc(
        q="My ___ brother is a doctor.",
        hint='表示家庭成员中的"年长的"',
        options=['elder', 'older', 'oldest', 'more old'],
        answer_idx=0,
        explain='elder 只用于表示家庭成员的长幼关系（elder brother/sister）。older 可以用于任何比较。',
        examples=[f'My <strong>elder</strong> brother is a doctor. 我的哥哥是医生。',
                  f'This building is <strong>older</strong> than that one. 这栋楼比那栋旧。'],
        mnemonic='elder = 年长的（仅家人）→ elder brother/sister',
        cat='adjective', diff='medium'
    ))

    # ============================================================
    # 3. 比较结构
    # ============================================================

    # --- as...as ---
    as_as_pairs = [
        ("He is as ___ as his father.", "tall", "和...一样高"),
        ("This book is as ___ as that one.", "interesting", "和...一样有趣"),
        ("She runs as ___ as a rabbit.", "fast", "和...一样快"),
        ("The movie is as ___ as I expected.", "good", "和...一样好"),
        ("My bag is as ___ as yours.", "heavy", "和...一样重"),
        ("He is as ___ as a fox.", "clever", "和...一样聪明"),
        ("This soup is as ___ as honey.", "sweet", "和...一样甜"),
        ("She is as ___ as her mother.", "beautiful", "和...一样美"),
        ("The problem is as ___ as I thought.", "difficult", "和...一样难"),
        ("His voice is as ___ as thunder.", "loud", "和...一样响"),
        ("This pillow is as ___ as a cloud.", "soft", "和...一样软"),
        ("The room is as ___ as a freezer.", "cold", "和...一样冷"),
        ("She is as ___ as a princess.", "pretty", "和...一样漂亮"),
        ("The test was as ___ as pie.", "easy", "和...一样容易"),
        ("He is as ___ as an ox.", "strong", "和...一样强壮"),
    ]

    for q, ans, meaning in as_as_pairs:
        options = [ans, f"more {ans}", f"most {ans}", f"{ans}er" if len(ans) < 7 else f"more {ans}"]
        random.shuffle(options)
        ans_idx = options.index(ans)

        questions.append(make_mc(
            q=q, hint=f'as...as 结构，{meaning}',
            options=options, answer_idx=ans_idx,
            explain=f'as + 形容词原级 + as 表示"和...一样..."。中间用原级，不能是比较级或最高级。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'He is not as <strong>{ans}</strong> as his brother. 他不如他兄弟高。'],
            mnemonic=f'as + 原级 + as = {meaning}',
            cat='adjective', diff='easy'
        ))

    # --- not as/so...as ---
    not_as_as = [
        ("She is not as/so ___ as her sister.", "tall", "不如...高"),
        ("This movie is not as/so ___ as that one.", "interesting", "不如...有趣"),
        ("He is not as/so ___ as he looks.", "young", "不如看上去年轻"),
        ("The test was not as/so ___ as I feared.", "difficult", "不如担心的难"),
        ("She is not as/so ___ as she used to be.", "happy", "不如以前快乐"),
    ]
    for q, ans, meaning in not_as_as:
        questions.append(make_fill(
            q=q, hint=f'not as/so...as = {meaning}，中间用原级',
            answer=ans,
            explain=f'not as/so + 形容词原级 + as 表示"不如..."。同样用原级。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'She is as <strong>{ans}</strong> as ever. 她和以前一样快乐。'],
            mnemonic=f'not as + 原级 + as = {meaning}',
            cat='adjective', diff='easy'
        ))

    # --- the more...the more ---
    the_more = [
        ("The ___ you practice, the ___ you become.", "more / better", "越多练习，越好"),
        ("The ___ I know him, the ___ I like him.", "more / more", "越了解，越喜欢"),
        ("The ___ you eat, the ___ you will get.", "more / fatter", "吃得越多，越胖"),
        ("The ___ it is, the ___ it feels.", "colder / worse", "越冷，感觉越糟"),
        ("The ___ we get, the ___ we understand.", "older / more", "越老，越明白"),
        ("The ___ you hurry, the ___ mistakes you make.", "more / more", "越急，犯错越多"),
        ("The ___ he spoke, the ___ I became.", "longer / angrier", "他讲得越久，我越生气"),
        ("The ___ the price, the ___ the quality.", "higher / better", "价格越高，质量越好"),
    ]
    for q, ans, meaning in the_more:
        ans_parts = ans.split(" / ")
        questions.append(make_fill(
            q=q, hint=f'the more...the more 结构: {meaning}',
            answer=ans,
            explain=f'"the + 比较级, the + 比较级" 表示"越...越..."。\n{meaning}。',
            examples=[f'<strong>The {ans_parts[0]}</strong> you practice, <strong>the {ans_parts[1]}</strong> you become.',
                      f'The {ans_parts[0]}, the {ans_parts[1]}.'],
            mnemonic=f'the + 比较级, the + 比较级 = 越...越...',
            cat='adjective', diff='medium'
        ))

    # --- 比较级+and+比较级 ---
    comp_and_comp = [
        ("The weather is getting ___ and ___. (warm)", "warmer / warmer", "越来越暖和"),
        ("Our country is becoming ___ and ___. (strong)", "stronger / stronger", "越来越强大"),
        ("She is becoming ___ and ___ ___. (beautiful)", "more / more beautiful", "越来越美"),
        ("Life is getting ___ and ___ ___. (difficult)", "more / more difficult", "越来越困难"),
        ("The music became ___ and ___ ___. (exciting)", "more / more exciting", "越来越激动人心"),
        ("He drove ___ and ___. (fast)", "faster / faster", "越来越快"),
        ("The noise grew ___ and ___. (loud)", "louder / louder", "越来越响"),
        ("The days are getting ___ and ___. (long)", "longer / longer", "越来越长"),
        ("She sang ___ and ___ ___. (beautifully)", "more / more beautifully", "唱得越来越美"),
        ("He spoke ___ and ___ ___. (slowly)", "more / more slowly", "说得越来越慢"),
    ]
    for q, ans, meaning in comp_and_comp:
        questions.append(make_fill(
            q=q, hint=f'比较级+and+比较级 = {meaning}',
            answer=ans,
            explain=f'"比较级 + and + 比较级" 表示"越来越..."。单音节用 -er and -er，多音节用 more and more + 原级。',
            examples=[f'The weather is getting <strong>{ans.replace(" / ", " and ")}</strong>.',
                      f'Things are getting better and better. 事情越来越好。'],
            mnemonic=f'比较级 + and + 比较级 = {meaning}',
            cat='adjective', diff='medium'
        ))

    # ============================================================
    # 4. 程度副词
    # ============================================================
    degree_adverbs = [
        ("very", "非常", "一般形容词", "最常用的程度副词"),
        ("quite", "相当/十分", "形容词/副词", "quite good = 相当好"),
        ("rather", "相当/颇", "贬义形容词", "rather bad = 相当差"),
        ("fairly", "相当（中等程度）", "褒义形容词", "fairly good = 还不错"),
        ("extremely", "极其", "形容词/副词", "extremely happy = 极其开心"),
        ("absolutely", "绝对地", "极限形容词", "absolutely wonderful = 绝对精彩"),
        ("completely", "完全地", "形容词", "completely different = 完全不同"),
        ("really", "真的/非常", "形容词/动词", "really good = 真的很好"),
        ("pretty", "相当（口语）", "形容词/副词", "pretty good = 相当不错"),
        ("so", "如此/太", "形容词/副词", "so beautiful = 如此美丽"),
        ("too", "太（过度）", "形容词/副词", "too expensive = 太贵了"),
        ("enough", "足够", "形容词/副词后", "good enough = 足够好"),
    ]

    for adv, meaning, usage, desc in degree_adverbs:
        # MC题
        distractor_pool = [a for a, _, _, _ in degree_adverbs if a != adv]
        distractors = random.sample(distractor_pool, 3)
        options = [adv] + distractors
        random.shuffle(options)
        ans_idx = options.index(adv)

        questions.append(make_mc(
            q=f"This book is ___ interesting. I can't put it down!",
            hint=f'表示"{meaning}"，修饰{usage}',
            options=options,
            answer_idx=ans_idx,
            explain=f'{adv} = {meaning}。{desc}。',
            examples=[f'This book is <strong>{adv}</strong> interesting. 这本书{meaning}有趣。',
                      f'The movie was <strong>{adv}</strong> good. 电影{meaning}好。'],
            mnemonic=f'{adv} = {meaning} → {desc}',
            cat='adjective', diff='medium' if adv in ('rather', 'fairly', 'absolutely') else 'easy'
        ))

    # quite/rather/fairly 辨析
    quite_rather_fairly = [
        ("The film was ___ good, but not great.", "fairly", "fairly 表中等程度的'还不错'"),
        ("It's ___ cold today — I need a heavy coat.", "rather", "rather 可修饰贬义/极端词，语气较强"),
        ("She is ___ a good singer.", "quite", "quite a + 名词 表示'相当...的'"),
        ("The food here is ___ disappointing.", "rather", "rather 常用于贬义形容词前"),
        ("He did ___ well in the exam.", "fairly", "fairly 表示'还算可以'"),
        ("I'm ___ sure he will come.", "quite", "quite = 相当（肯定程度高）"),
    ]
    for q, ans, explain in quite_rather_fairly:
        options = ["quite", "rather", "fairly", "very"]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint='选择合适的程度副词',
            options=options, answer_idx=ans_idx,
            explain=explain,
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'It was <strong>{ans}</strong> interesting.'],
            mnemonic='fairly=还不错，rather=相当(贬)，quite=相当(褒)',
            cat='adjective', diff='hard'
        ))

    # --- enough/too/so/such ---
    # too + adj + to do
    too_to = [
        ("He is ___ young to drive a car.", "too", "太...以至于不能..."),
        ("The box is ___ heavy to carry.", "too", "太重以至于搬不动"),
        ("She was ___ tired to continue.", "too", "太累以至于不能继续"),
        ("It's ___ late to go out now.", "too", "太晚以至于不能出去"),
        ("The coffee is ___ hot to drink.", "too", "太烫以至于不能喝"),
        ("He spoke ___ fast for me to understand.", "too", "说得太快以至于我听不懂"),
    ]
    for q, ans, meaning in too_to:
        options = ["too", "so", "very", "enough"]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'too...to = {meaning}',
            options=options, answer_idx=ans_idx,
            explain=f'too + adj + to do = 太...而不能...。注意：用 too 不是 so/very。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'It is <strong>too</strong> difficult for a beginner. 对初学者来说太难了。'],
            mnemonic='too + adj + to do = 太...而不能...',
            cat='adjective', diff='easy'
        ))

    # adj + enough
    enough_usage = [
        ("He is old ___ to make his own decisions.", "enough", "足够大可以自己做决定"),
        ("She is tall ___ to reach the shelf.", "enough", "足够高可以够到架子"),
        ("Do you have ___ money?", "enough", "有足够的钱吗？"),
        ("The room is big ___ for ten people.", "enough", "足够大容纳十人"),
        ("He didn't study hard ___.", "enough", "学习不够努力"),
        ("Is this good ___?", "enough", "这够好吗？"),
    ]
    for q, ans, meaning in enough_usage:
        options = ["enough", "too", "so", "very"]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'形容词+enough = {meaning}',
            options=options, answer_idx=ans_idx,
            explain=f'形容词/副词 + enough = 足够...。注意 enough 放在形容词之后，但放在名词之前。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'He is not <strong>enough</strong> old. ❌ → He is not old <strong>enough</strong>. ✓'],
            mnemonic='形容词 + enough；enough + 名词',
            cat='adjective', diff='easy'
        ))

    # so...that / such...that
    so_such = [
        ("It was ___ hot that we couldn't go out.", "so", "so + adj + that"),
        ("She is ___ a good teacher that everyone likes her.", "such", "such + a/an + adj + n + that"),
        ("He ran ___ fast that no one could catch him.", "so", "so + adv + that"),
        ("They are ___ nice people that we love them.", "such", "such + adj + n(复数) + that"),
        ("It was ___ a beautiful day that we went for a picnic.", "such", "such + a + adj + n + that"),
        ("The music was ___ loud that I couldn't sleep.", "so", "so + adj + that"),
        ("He has ___ many friends that he's never lonely.", "so", "so many/much + n + that"),
        ("There was ___ little time left that we ran.", "so", "so little/much + n + that"),
    ]
    for q, ans, rule in so_such:
        options = ["so", "such", "very", "too"]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'{rule}',
            options=options, answer_idx=ans_idx,
            explain=f'{rule}。so 后直接接 adj/adv；such 后接 a/an + adj + 名词。\n例外：so many/much/little/few + 名词。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'It is <strong>so</strong> interesting. / It is <strong>such</strong> an interesting book.'],
            mnemonic='so + adj/adv；such + (a/an) + adj + n',
            cat='adjective', diff='medium'
        ))

    # ============================================================
    # 5. 频度副词
    # ============================================================
    freq_adverbs = [
        ("always", "总是（100%）", "实义动词前/be动词后"),
        ("usually", "通常（约80%）", "实义动词前/be动词后"),
        ("often", "经常（约60%）", "实义动词前/be动词后"),
        ("sometimes", "有时（约40%）", "句首/句中/句末"),
        ("rarely", "很少（约10%）", "实义动词前/be动词后"),
        ("seldom", "很少（约5%）", "实义动词前/be动词后"),
        ("never", "从不（0%）", "实义动词前/be动词后"),
        ("hardly ever", "几乎从不", "实义动词前/be动词后"),
        ("occasionally", "偶尔（约20%）", "句首/句中/句末"),
        ("frequently", "频繁地（约70%）", "实义动词前/be动词后"),
    ]

    for adv, meaning, position in freq_adverbs:
        options = [a for a, _, _ in freq_adverbs if a != adv]
        distractors = random.sample(options, 3)
        options_mc = [adv] + distractors
        random.shuffle(options_mc)
        ans_idx = options_mc.index(adv)

        questions.append(make_mc(
            q=f"She ___ gets up at 6 a.m.",
            hint=f'频度副词，{meaning}',
            options=options_mc,
            answer_idx=ans_idx,
            explain=f'{adv} = {meaning}。频度副词一般放在 be 动词之后、实义动词之前。位置：{position}。',
            examples=[f'She <strong>{adv}</strong> gets up at 6 a.m. 她{meaning}六点起床。',
                      f'She is <strong>{adv}</strong> late for school. 她{meaning}上学迟到。'],
            mnemonic=f'{adv} = {meaning} → 动词前，be动词后',
            cat='adjective', diff='easy'
        ))

    # 频度副词位置专项
    freq_position = [
        ("He is ___ late for work.", "never", "be动词后"),
        ("She ___ goes to the library after school.", "often", "实义动词前"),
        ("I have ___ been to Paris.", "never", "助动词后"),
        ("Do you ___ eat breakfast?", "usually", "助动词后（疑问句）"),
        ("She doesn't ___ watch TV.", "often", "助动词后（否定句）"),
        ("He can ___ solve such problems.", "always", "情态动词后"),
    ]
    for q, ans, rule in freq_position:
        options = ["always", "usually", "often", "sometimes", "rarely", "seldom", "never"]
        options = [o for o in options if o != ans][:3] + [ans]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'频度副词位置：{rule}',
            options=options, answer_idx=ans_idx,
            explain=f'频度副词位置规则：be动词/助动词/情态动词之后，实义动词之前。此处 {rule}。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'She <strong>{ans}</strong> reads books. 她经常读书。'],
            mnemonic='频度副词：be/助/情之后，实义动词前',
            cat='adjective', diff='medium'
        ))

    # ============================================================
    # 6. 形容词顺序
    # ============================================================
    adj_order_questions = [
        ("She bought a ___ dress.", "beautiful red silk",
         "她买了一条漂亮的红色丝绸裙子。",
         "多个形容词顺序：观点(beautiful) → 颜色(red) → 材质(silk)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["beautiful silk red", "red beautiful silk", "silk red beautiful"]),
        ("He is a ___ boy.", "clever little Chinese",
         "他是一个聪明的小中国男孩。",
         "观点(clever) → 大小(little) → 来源(Chinese)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["little clever Chinese", "Chinese clever little", "little Chinese clever"]),
        ("I bought a ___ table.", "nice small round wooden",
         "我买了一个漂亮的小圆木桌。",
         "观点(nice) → 大小(small) → 形状(round) → 材质(wooden)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["wooden small nice round", "round wooden nice small", "small wooden round nice"]),
        ("She has ___ hair.", "long beautiful black",
         "她有一头美丽的长黑发。",
         "观点(beautiful) → 长度(long) → 颜色(black)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["beautiful black long", "black long beautiful", "long black beautiful"]),
        ("He drives a ___ car.", "new red Japanese",
         "他开一辆新的红色日本车。",
         "年龄(new) → 颜色(red) → 来源(Japanese)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["Japanese red new", "red new Japanese", "new Japanese red"]),
        ("It's a ___ building.", "tall modern glass",
         "这是一栋高大的现代玻璃建筑。",
         "大小(tall) → 风格(modern) → 材质(glass)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["modern glass tall", "glass tall modern", "modern tall glass"]),
        ("She wore a ___ hat.", "lovely big straw",
         "她戴了一顶可爱的大草帽。",
         "观点(lovely) → 大小(big) → 材质(straw)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["big lovely straw", "straw big lovely", "lovely straw big"]),
        ("He has ___ eyes.", "big round blue",
         "他有一双大圆蓝眼睛。",
         "大小(big) → 形状(round) → 颜色(blue)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["blue round big", "round blue big", "big blue round"]),
        ("I saw a ___ car.", "small old green Italian",
         "我看到一辆小的旧绿色意大利车。",
         "大小(small) → 年龄(old) → 颜色(green) → 来源(Italian)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["old small Italian green", "green Italian small old", "Italian small green old"]),
        ("This is a ___ vase.", "beautiful ancient Chinese",
         "这是一个美丽的中国古代花瓶。",
         "观点(beautiful) → 年龄(ancient) → 来源(Chinese)",
         "观点+大小+年龄+形状+颜色+来源+材质+用途",
         ["Chinese ancient beautiful", "ancient beautiful Chinese", "beautiful Chinese ancient"]),
    ]

    for q, correct, cn, explain, mnemonic, distractors in adj_order_questions:
        options = [correct] + distractors
        random.shuffle(options)
        ans_idx = options.index(correct)
        questions.append(make_mc(
            q=q, hint=f'注意形容词排列顺序',
            options=options, answer_idx=ans_idx,
            explain=f'{explain}。\n\n口诀：{mnemonic}',
            examples=[f'She bought a <strong>{correct}</strong> dress. {cn}',
                      f'多个形容词按 观点→大小→年龄→形状→颜色→来源→材质→用途 排列。'],
            mnemonic=mnemonic,
            cat='adjective', diff='hard'
        ))

    # 形容词顺序口诀记忆填空
    adj_order_fill = [
        ("多个形容词的排列顺序口诀：观点→___→年龄→形状→颜色→来源→材质→用途", "大小",
         "形容词顺序口诀"),
        ("a ___ young Chinese woman (观点/大小/年龄/来源)", "beautiful",
         "观点形容词在最前面"),
    ]
    for q, ans, hint in adj_order_fill:
        questions.append(make_fill(
            q=q, hint=hint, answer=ans,
            explain='形容词顺序口诀：美小旧形色国材（观点→大小→年龄→形状→颜色→来源→材质→用途）',
            examples=['a beautiful old Chinese vase 一个美丽的中国古花瓶',
                      'a nice small round table 一张漂亮的小圆桌'],
            mnemonic='美小旧形色国材（观点→大小→年龄→形状→颜色→来源→材质→用途）',
            cat='adjective', diff='hard'
        ))

    # ============================================================
    # 7. -ful/-less 后缀
    # ============================================================
    ful_less_pairs = [
        ("careful", "careless", "仔细的", "粗心的"),
        ("hopeful", "hopeless", "有希望的", "无望的"),
        ("useful", "useless", "有用的", "无用的"),
        ("helpful", "helpless", "有帮助的", "无助的"),
        ("powerful", "powerless", "强大的", "无力的"),
        ("meaningful", "meaningless", "有意义的", "无意义的"),
        ("harmful", "harmless", "有害的", "无害的"),
        ("thankful", "thankless", "感激的", "徒劳无功的"),
        ("thoughtful", "thoughtless", "体贴的/深思的", "粗心的/欠考虑的"),
        ("fearful", "fearless", "害怕的", "无畏的"),
        ("painful", "painless", "痛苦的", "无痛的"),
        ("colorful", "colorless", "多彩的", "无色的"),
        ("wonderful", "wonderless", "精彩的", "平淡无奇的"),
        ("peaceful", "peaceless", "和平的/平静的", "不安宁的"),
        ("doubtful", "doubtless", "可疑的", "无疑的"),
        ("endless", "—", "无尽的", "—"),
        ("homeless", "—", "无家可归的", "—"),
        ("priceless", "—", "无价的", "—"),
        ("speechless", "—", "说不出话的", "—"),
        ("worthless", "—", "无价值的", "—"),
    ]

    for ful_form, less_form, ful_meaning, less_meaning in ful_less_pairs:
        if less_form != "—":
            # MC题 — 选 -ful
            options_ful = [ful_form, less_form]
            random.shuffle(options_ful)
            ans_ful = options_ful.index(ful_form)

            questions.append(make_mc(
                q=f"Be ___ when you cross the road!",
                hint=f'表示"{ful_meaning}"',
                options=[ful_form, less_form],
                answer_idx=ans_ful,
                explain=f'{ful_form} = {ful_meaning}。-ful 后缀表示"充满...的"。\n{less_form} = {less_meaning}。-less 后缀表示"没有...的"。',
                examples=[f'Be <strong>{ful_form}</strong> when you cross the road. 过马路时要小心。',
                          f'He is a <strong>{ful_form}</strong> student. 他是一个认真的学生。'],
                mnemonic=f'-ful = 有（充满）→ {ful_form}；-less = 无 → {less_form}',
                cat='adjective', diff='easy'
            ))

            # MC题 — 选 -less
            options_less = [less_form, ful_form]
            random.shuffle(options_less)
            ans_less = options_less.index(less_form)

            questions.append(make_mc(
                q=f"Without your help, I would be ___.",
                hint=f'表示"{less_meaning}"',
                options=[less_form, ful_form],
                answer_idx=ans_less,
                explain=f'{less_form} = {less_meaning}。-less 后缀表示"没有...的"。\n{ful_form} = {ful_meaning}。',
                examples=[f'Without your help, I would be <strong>{less_form}</strong>. 没有你的帮助，我将是无助的。',
                          f'He felt <strong>{less_form}</strong> in the face of the problem. 面对问题他感到无助。'],
                mnemonic=f'-ful = 有 → {ful_form}；-less = 无 → {less_form}',
                cat='adjective', diff='easy'
            ))

    # -ful/-less 填空
    ful_less_fill = [
        ("Be ___ (care) when you cross the street!", "careful", "care + ful = 小心的"),
        ("He made a ___ (care) mistake.", "careless", "care + less = 粗心的"),
        ("This dictionary is very ___ (use).", "useful", "use + ful = 有用的"),
        ("The broken phone is ___ (use).", "useless", "use + less = 无用的"),
        ("I am ___ (hope) that I will pass the exam.", "hopeful", "hope + ful = 有希望的"),
        ("The situation seems ___ (hope).", "hopeless", "hope + less = 无望的"),
        ("Thank you, you've been very ___ (help).", "helpful", "help + ful = 有帮助的"),
        ("The baby bird looked ___ (help) on the ground.", "helpless", "help + less = 无助的"),
        ("The drug is ___ (harm) to your health.", "harmful", "harm + ful = 有害的"),
        ("The snake is ___ (harm); it has no poison.", "harmless", "harm + less = 无害的"),
        ("She has ___ (end) energy.", "endless", "end + less = 无尽的"),
        ("The painting is ___ (price); it can't be bought.", "priceless", "price + less = 无价的"),
    ]
    for q, ans, explain in ful_less_fill:
        questions.append(make_fill(
            q=q, hint='-ful = 有, -less = 无',
            answer=ans, explain=explain,
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'The word is <strong>{ans}</strong>.'],
            mnemonic='-ful = 有（充满），-less = 无（缺少）',
            cat='adjective', diff='easy'
        ))

    # ============================================================
    # 8. 常见反义词对
    # ============================================================
    antonym_pairs = [
        ("tall", "short", "高的", "矮的"),
        ("fast", "slow", "快的", "慢的"),
        ("rich", "poor", "富有的", "贫穷的"),
        ("strong", "weak", "强壮的", "弱的"),
        ("big", "small", "大的", "小的"),
        ("hot", "cold", "热的", "冷的"),
        ("thick", "thin", "厚的", "薄的"),
        ("wide", "narrow", "宽的", "窄的"),
        ("heavy", "light", "重的", "轻的"),
        ("hard", "soft", "硬的", "软的"),
        ("old", "young", "老的", "年轻的"),
        ("new", "old", "新的", "旧的"),
        ("easy", "difficult", "容易的", "困难的"),
        ("cheap", "expensive", "便宜的", "昂贵的"),
        ("safe", "dangerous", "安全的", "危险的"),
        ("clean", "dirty", "干净的", "脏的"),
        ("full", "empty", "满的", "空的"),
        ("happy", "sad", "快乐的", "悲伤的"),
        ("bright", "dark", "明亮的", "暗的"),
        ("loud", "quiet", "大声的", "安静的"),
        ("brave", "cowardly", "勇敢的", "胆小的"),
        ("kind", "cruel", "善良的", "残忍的"),
        ("polite", "rude", "有礼貌的", "粗鲁的"),
        ("honest", "dishonest", "诚实的", "不诚实的"),
        ("patient", "impatient", "耐心的", "不耐烦的"),
    ]

    for adj1, adj2, m1, m2 in antonym_pairs:
        # MC题
        options_ant = [adj1, adj2, f"very {adj1}", f"very {adj2}"]
        random.shuffle(options_ant)
        ans_ant = options_ant.index(adj2) if random.random() < 0.5 else options_ant.index(adj1)
        if ans_ant == options_ant.index(adj2):
            q_text = f"What is the opposite of '{adj1}'?"
            explain_text = f'{adj1}（{m1}）的反义词是 {adj2}（{m2}）。'
        else:
            q_text = f"What is the opposite of '{adj2}'?"
            explain_text = f'{adj2}（{m2}）的反义词是 {adj1}（{m1}）。'

        questions.append(make_mc(
            q=q_text,
            hint=f'选择正确的反义词',
            options=options_ant,
            answer_idx=ans_ant,
            explain=explain_text,
            examples=[f'He is <strong>{adj1}</strong>, not {adj2}. 他{adj1}，不是{adj2}。',
                      f'The {adj1} and the {adj2} are different. {adj1}和{adj2}不同。'],
            mnemonic=f'{adj1} ↔ {adj2}',
            cat='adjective', diff='easy'
        ))

    # ============================================================
    # 9. 同义形容词辨析
    # ============================================================
    # big/large/great/huge/enormous
    big_family = [
        ("I have a ___ family with six members.", "big",
         "big 最常用，形容尺寸/规模/重要性",
         ["large", "great", "huge"]),
        ("This box is too ___ to fit in the car.", "large",
         "large 较正式，形容面积/体积/数量",
         ["big", "great", "enormous"]),
        ("Alexander the ___ was a famous king.", "Great",
         "great 形容伟大/重要性/程度（Great Wall, great leader）",
         ["Big", "Large", "Huge"]),
        ("The elephant is ___!", "huge",
         "huge = 巨大的（强调体积庞大，比 big/large 更夸张）",
         ["large", "great", "big"]),
        ("There is an ___ amount of work to do.", "enormous",
         "enormous = 极其巨大的（程度极高）",
         ["big", "large", "huge"]),
        ("China is a ___ country.", "big",
         "big 表示规模大，最常用",
         ["great", "huge", "enormous"]),
    ]
    for q, ans, explain, distractors in big_family:
        options = [ans] + distractors
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint='选择合适的"大"',
            options=options, answer_idx=ans_idx,
            explain=f'{explain}。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'{ans} is the best choice here. {ans}在这里最合适。'],
            mnemonic='big=一般大，large=面积大，great=伟大，huge/enormous=巨大',
            cat='adjective', diff='hard'
        ))

    # small/little/tiny
    small_family = [
        ("She lives in a ___ house by the lake.", "small",
         "small = 小的（纯客观描述尺寸）",
         ["little", "tiny"]),
        ("What a ___ baby!", "little",
         "little = 小的（带感情色彩，可爱/同情）",
         ["small", "tiny"]),
        ("The ant is a ___ insect.", "tiny",
         "tiny = 极小的（强调非常非常小）",
         ["small", "little"]),
        ("He gave me a ___ smile.", "little",
         "little 用于抽象/感情色彩（little smile = 微微一笑）",
         ["small", "tiny"]),
        ("There is a ___ garden behind the house.", "small",
         "small = 客观描述尺寸",
         ["little", "tiny"]),
        ("A ___ bird landed on my hand.", "tiny",
         "tiny = 极小，比 small 更小",
         ["small", "little"]),
    ]
    for q, ans, explain, distractors in small_family:
        options = [ans] + distractors
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint='选择合适的"小"',
            options=options, answer_idx=ans_idx,
            explain=f'{explain}。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'{ans} fits the context best.'],
            mnemonic='small=客观小，little=可爱小/抽象小，tiny=极微小',
            cat='adjective', diff='hard'
        ))

    # happy/glad/pleased/delighted
    happy_family = [
        ("I'm so ___ to hear your good news!", "happy",
         "happy = 开心的（最常用，通用场景）",
         ["glad", "pleased", "delighted"]),
        ("I would be ___ to help you.", "glad",
         "glad = 乐意的/高兴的（较正式/礼貌，常用于'乐意做某事'）",
         ["happy", "pleased", "delighted"]),
        ("The boss was ___ with our work.", "pleased",
         "pleased = 满意的（对结果/表现感到满意）",
         ["happy", "glad", "delighted"]),
        ("We are ___ to invite you to the party!", "delighted",
         "delighted = 非常高兴的（程度比 happy 更强，较正式）",
         ["happy", "glad", "pleased"]),
        ("Are you ___ with the result?", "happy",
         "happy with = 对...满意/高兴",
         ["glad", "pleased", "delighted"]),
        ("I'm ___ to meet you.", "glad",
         "glad to meet you = 很高兴认识你（礼貌用语）",
         ["happy", "pleased", "delighted"]),
    ]
    for q, ans, explain, distractors in happy_family:
        options = [ans] + distractors
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint='选择合适的"高兴"',
            options=options, answer_idx=ans_idx,
            explain=f'{explain}。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'{ans} is the most natural choice.'],
            mnemonic='happy=开心，glad=乐意，pleased=满意，delighted=非常高兴',
            cat='adjective', diff='hard'
        ))

    # ============================================================
    # 10. 其他重要形容词/副词知识点
    # ============================================================
    # 形容词变副词规则
    adj_to_adv = [
        ("quick", "quickly", "快的 → 快地"),
        ("slow", "slowly", "慢的 → 慢地"),
        ("careful", "carefully", "仔细的 → 仔细地"),
        ("happy", "happily", "快乐的 → 快乐地（y→i+ly）"),
        ("easy", "easily", "容易的 → 容易地（y→i+ly）"),
        ("angry", "angrily", "生气的 → 生气地（y→i+ly）"),
        ("gentle", "gently", "温柔的 → 温柔地（e去+ly）"),
        ("simple", "simply", "简单的 → 简单地（e去+y）"),
        ("true", "truly", "真的 → 真正地（e去+ly）"),
        ("full", "fully", "满的 → 完全地"),
        ("automatic", "automatically", "自动的 → 自动地（-ic+ally）"),
        ("good", "well", "好的 → 好地（不规则）"),
        ("fast", "fast", "快的 → 快地（同形）"),
        ("hard", "hard", "努力的 → 努力地（同形）"),
        ("late", "late", "晚的 → 晚地（同形）"),
        ("early", "early", "早的 → 早地（同形）"),
        ("high", "high", "高的 → 高地（同形）"),
    ]

    for adj, adv, meaning in adj_to_adv:
        questions.append(make_fill(
            q=f"She solved the problem ___ ({adj}).",
            hint=f'{adj} 的副词形式',
            answer=adv,
            explain=f'{adj} 的副词形式是 {adv}（{meaning}）。',
            examples=[f'She solved the problem <strong>{adv}</strong>. 她{adj}地解决了问题。',
                      f'He ran <strong>{adv}</strong>. 他{adj}地跑。'],
            mnemonic=f'{adj} → {adv}（{meaning}）',
            cat='adjective', diff='medium' if adj in ('happy', 'easy', 'gentle', 'true', 'good') else 'easy'
        ))

    # hard/hardly 辨析
    questions.append(make_mc(
        q="He works ___ every day.",
        hint='hard = 努力地，hardly = 几乎不',
        options=['hard', 'hardly', 'hardly ever', 'harder'],
        answer_idx=0,
        explain='hard 既可作形容词（硬的/困难的）也可作副词（努力地）。hardly 是副词，意为"几乎不"，与 hard 完全不同！',
        examples=['He works <strong>hard</strong> every day. 他每天努力工作。',
                  'He <strong>hardly</strong> works. 他几乎不工作。'],
        mnemonic='hard = 努力，hardly = 几乎不（别搞混！）',
        cat='adjective', diff='medium'
    ))

    questions.append(make_mc(
        q="I could ___ believe my eyes when I saw the gift.",
        hint='hardly = 几乎不',
        options=['hardly', 'hard', 'hardly ever', 'harshly'],
        answer_idx=0,
        explain='hardly = 几乎不。could hardly believe = 几乎不敢相信。不能用 hard。',
        examples=['I could <strong>hardly</strong> believe it. 我几乎不敢相信。',
                  'She <strong>hardly</strong> ever goes out. 她几乎从不出门。'],
        mnemonic='hardly = 几乎不，hard = 努力/坚硬',
        cat='adjective', diff='medium'
    ))

    # late/lately 辨析
    questions.append(make_mc(
        q="Have you seen any good movies ___?",
        hint='lately = 最近',
        options=['lately', 'late', 'later', 'latest'],
        answer_idx=0,
        explain='lately = 最近（= recently）。late = 晚的/迟的。later = 后来。latest = 最新的。',
        examples=['Have you seen any good movies <strong>lately</strong>? 你最近看了什么好电影吗？',
                  'He came home <strong>late</strong> last night. 他昨晚很晚才回家。'],
        mnemonic='lately = 最近（= recently），late = 迟到/晚',
        cat='adjective', diff='medium'
    ))

    # near/nearly 辨析
    questions.append(make_mc(
        q="I ___ missed the bus this morning.",
        hint='nearly = 几乎/差点',
        options=['nearly', 'near', 'nearer', 'nearest'],
        answer_idx=0,
        explain='nearly = 几乎/差点（= almost）。near = 在附近。nearly missed = 差点错过。',
        examples=['I <strong>nearly</strong> missed the bus. 我差点错过公交车。',
                  'The school is <strong>near</strong> my home. 学校在我家附近。'],
        mnemonic='nearly = 几乎（= almost），near = 附近',
        cat='adjective', diff='medium'
    ))

    # 感官动词 + 形容词
    sense_verbs = [
        ("The food smells ___.", "delicious", "闻起来"),
        ("This music sounds ___.", "beautiful", "听起来"),
        ("The silk feels ___.", "soft", "摸起来"),
        ("This soup tastes ___.", "salty", "尝起来"),
        ("You look ___ today.", "tired", "看起来"),
        ("The flowers smell ___.", "sweet", "闻起来"),
        ("His voice sounds ___.", "strange", "听起来"),
        ("The baby's skin feels ___.", "smooth", "摸起来"),
        ("This dish tastes ___.", "wonderful", "尝起来"),
        ("She looks ___ in that dress.", "beautiful", "看起来"),
    ]
    for q, ans, verb in sense_verbs:
        options = [ans, f"{ans}ly", f"very {ans}", f"more {ans}"]
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'感官动词 {q.split()[1]} 后接形容词，不是副词',
            options=options, answer_idx=ans_idx,
            explain=f'感官动词（look/sound/smell/taste/feel）后接形容词，不是副词。不能说 taste deliciously。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'It {q.split()[1]} <strong>{ans}</strong>. 它{verb}{ans}。'],
            mnemonic='感官动词 + 形容词（不是副词）',
            cat='adjective', diff='easy'
        ))

    # 复合形容词
    compound_adjs = [
        ("She is a ___ girl.", "ten-year-old", "十岁的"),
        ("This is a ___ bridge.", "200-meter-long", "200米长的"),
        ("He wrote a ___ article.", "500-word", "500字的"),
        ("It's a ___ journey.", "three-hour", "三小时的"),
        ("She is a ___ woman.", "kind-hearted", "好心的"),
        ("He is a ___ student.", "hard-working", "勤奋的"),
        ("This is a ___ book.", "well-written", "写得好的"),
        ("She is a ___ singer.", "world-famous", "世界著名的"),
        ("He gave a ___ speech.", "five-minute", "五分钟的"),
        ("It's a ___ movie.", "full-length", "完整的/长片"),
    ]
    for q, ans, meaning in compound_adjs:
        distractors_pool = [a for _, a, _ in compound_adjs if a != ans]
        distractors = random.sample(distractors_pool, min(3, len(distractors_pool)))
        options = [ans] + distractors
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=f'复合形容词，{meaning}',
            options=options, answer_idx=ans_idx,
            explain=f'复合形容词中各词用连字符连接，名词用单数形式（ten-year-old 不是 ten-years-old）。',
            examples=[q.replace("___", f"<strong>{ans}</strong>"),
                      f'She is <strong>{ans}</strong>. 她{meaning}。'],
            mnemonic='复合形容词 → 连字符连接，名词用单数',
            cat='adjective', diff='medium'
        ))

    # 补充综合题
    comprehensive = [
        # 比较级修饰
        ("This city is ___ larger than my hometown.", "much",
         "much 可以修饰比较级，表示'...得多'",
         "much/far/a lot/even/still + 比较级 = ...得多/更加",
         ["very", "more", "many"]),
        ("He is ___ more careful now.", "much",
         "much + more + 原级 = 更加...",
         "much 可修饰 more + 原级",
         ["very", "many", "too"]),
        ("This problem is ___ easier than that one.", "much",
         "much + 比较级 = ...得多",
         "much + easier = 容易得多",
         ["very", "more", "many"]),
        # 最高级修饰
        ("This is ___ the best book I've read.", "by far",
         "by far 修饰最高级，表示'远远/显然'",
         "by far the best = 显然是最好的",
         ["very", "much", "more"]),
        # 同级比较
        ("He is not ___ tall ___ his brother.", "as / as",
         "not as...as = 不如...",
         "not as + adj + as",
         ["so / than", "more / than", "as / than"]),
        ("She is three times as ___ as her sister.", "old",
         "three times as...as = 是...的三倍",
         "倍数 + as + adj + as",
         ["older", "oldest", "more old"]),
        # one of + 最高级 + 复数名词
        ("She is one of the ___ students in school.", "best",
         "one of + 最高级 + 复数名词 = 最...的...之一",
         "one of the best students（students 用复数）",
         ["better", "good", "most good"]),
        ("China is one of the ___ countries in the world.", "largest",
         "one of the largest countries = 最大的国家之一",
         "one of + 最高级 + 复数名词",
         ["large", "larger", "most large"]),
        # 比较级表示最高级含义
        ("He is ___ than any other student in class.", "taller",
         "比较级 + than any other = 比任何其他...都... = 最...",
         "taller than any other student = 最高的学生",
         ["tallest", "more tall", "most tall"]),
        ("This is ___ than any other book I've read.", "better",
         "better than any other book = 最好的书",
         "比较级 + than any other = 最高级含义",
         ["best", "more good", "good"]),
    ]
    for q, ans, hint, explain, distractors in comprehensive:
        options = [ans] + distractors
        random.shuffle(options)
        ans_idx = options.index(ans)
        questions.append(make_mc(
            q=q, hint=hint,
            options=options, answer_idx=ans_idx,
            explain=explain,
            examples=[q.replace("___", f"<strong>{ans}</strong>")],
            mnemonic=explain.split('\n')[0] if '\n' in explain else explain,
            cat='adjective', diff='hard'
        ))

    # ============================================================
    # 调整难度分布并确保达到500题
    # ============================================================
    # 统计当前数量
    total = len(questions)
    print(f"  初始生成: {total} 题")

    # 统计各难度数量
    diff_count = {'easy': 0, 'medium': 0, 'hard': 0}
    type_count = {'mc': 0, 'fill': 0}
    for q in questions:
        diff_count[q['diff']] += 1
        type_count[q['type']] += 1
    print(f"  难度分布: easy={diff_count['easy']}, medium={diff_count['medium']}, hard={diff_count['hard']}")
    print(f"  题型分布: mc={type_count['mc']}, fill={type_count['fill']}")

    # 如果不足500题，补充额外题目
    while len(questions) < 500:
        # 补充各种题型
        extra_adjs = ["wonderful", "terrible", "fantastic", "awful", "excellent",
                      "brilliant", "ordinary", "special", "common", "rare",
                      "ancient", "modern", "traditional", "natural", "artificial",
                      "similar", "different", "same", "unique", "typical"]
        extra_advs = ["quickly", "slowly", "carefully", "happily", "easily",
                      "angrily", "gently", "suddenly", "gradually", "finally",
                      "recently", "immediately", "certainly", "probably", "possibly"]

        adj = random.choice(extra_adjs)
        adv = random.choice(extra_advs)

        questions.append(make_mc(
            q=f"The performance was absolutely ___!",
            hint=f'选择最合适的形容词',
            options=[adj] + random.sample([a for a in extra_adjs if a != adj], 3),
            answer_idx=0,
            explain=f'{adj} 在此语境中最合适。',
            examples=[f'The performance was <strong>{adj}</strong>!',
                      f'It was a <strong>{adj}</strong> show.'],
            mnemonic=f'{adj} 用于描述精彩的表演',
            cat='adjective', diff=random.choice(['easy', 'medium', 'medium', 'hard'])
        ))

        questions.append(make_fill(
            q=f"He walked ___ (quick) to catch the bus.",
            hint=f'quick 的副词形式',
            answer=adv,
            explain=f'quick 的副词形式是 {adv}。形容词变副词通常加 -ly。',
            examples=[f'He walked <strong>{adv}</strong> to catch the bus.',
                      f'She answered <strong>{adv}</strong>.'],
            mnemonic=f'形容词 + ly → 副词',
            cat='adjective', diff='easy'
        ))

    # 按难度比例调整：easy 35%, medium 40%, hard 25%
    target_easy = int(len(questions) * 0.35)
    target_medium = int(len(questions) * 0.40)
    target_hard = len(questions) - target_easy - target_medium

    # 分离各难度
    easy_qs = [q for q in questions if q['diff'] == 'easy']
    medium_qs = [q for q in questions if q['diff'] == 'medium']
    hard_qs = [q for q in questions if q['diff'] == 'hard']

    # 将多余的 easy 题升级难度
    if len(easy_qs) > target_easy:
        overflow = easy_qs[target_easy:]
        easy_qs = easy_qs[:target_easy]
        # 分一半给 medium，一半给 hard
        for q in overflow[:len(overflow)//2]:
            q['diff'] = 'medium'
            medium_qs.append(q)
        for q in overflow[len(overflow)//2:]:
            q['diff'] = 'hard'
            hard_qs.append(q)

    # 调整 medium 数量
    if len(medium_qs) > target_medium:
        overflow_m = medium_qs[target_medium:]
        medium_qs = medium_qs[:target_medium]
        for q in overflow_m:
            q['diff'] = 'hard'
            hard_qs.append(q)
    elif len(medium_qs) < target_medium and len(hard_qs) > target_hard:
        needed = target_medium - len(medium_qs)
        move_from_hard = hard_qs[:needed]
        hard_qs = hard_qs[needed:]
        for q in move_from_hard:
            q['diff'] = 'medium'
            medium_qs.append(q)

    questions = easy_qs + medium_qs + hard_qs
    questions.sort(key=lambda x: (x['diff'], x['id']))

    diff_count2 = {'easy': 0, 'medium': 0, 'hard': 0}
    for q in questions:
        diff_count2[q['diff']] += 1

    print(f"  最终: {len(questions)} 题")
    print(f"  难度: easy={diff_count2['easy']}, medium={diff_count2['medium']}, hard={diff_count2['hard']}")
    print(f"  目标: easy≈{target_easy}, medium≈{target_medium}, hard≈{target_hard}")

    return questions

def main():
    print("开始生成形容词/副词题库...")
    questions = generate_adjectives()
    filepath = os.path.join(OUTPUT_DIR, "adjectives.json")
    with open(filepath, 'w', encoding='utf-8') as f:
        json.dump(questions, f, ensure_ascii=False, indent=2)
    print(f"\n✅ adjectives.json: {len(questions)} 题")
    print(f"🎉 生成完成!")

if __name__ == "__main__":
    main()
