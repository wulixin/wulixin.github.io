#!/usr/bin/env python3
"""
扩展冠词题库：从 117 题扩展到 400-600 题
覆盖：a/an 用法、定冠词 the、零冠词、固定搭配、冠词辨析
"""
import json, random, os

random.seed(42)

OUT = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUT, exist_ok=True)

# 加载现有题目，获取最大 ID
existing_file = os.path.join(OUT, "articles.json")
existing = []
max_id = 0
if os.path.exists(existing_file):
    with open(existing_file, 'r', encoding='utf-8') as f:
        existing = json.load(f)
    max_id = max(q['id'] for q in existing) if existing else 0

_id_counter = [max_id]

def nid():
    _id_counter[0] += 1
    return _id_counter[0]

def mc(q, hint, options, answer_idx, explain, examples, mnemonic, cat='article', diff='easy'):
    return {
        "id": nid(), "cat": cat, "diff": diff, "type": "mc",
        "q": q, "hint": hint, "options": options, "answer": answer_idx,
        "explain": explain, "examples": examples, "mnemonic": mnemonic
    }

def fl(q, hint, answer, explain, examples, mnemonic, cat='article', diff='easy'):
    return {
        "id": nid(), "cat": cat, "diff": diff, "type": "fill",
        "q": q, "hint": hint, "answer": answer,
        "explain": explain, "examples": examples, "mnemonic": mnemonic
    }

def gen_articles():
    Q = []

    # ================================================================
    # 1. a vs an — 大量练习 (元音音素 vs 辅音音素)
    # ================================================================
    a_words = [
        ("book", "书"), ("pen", "笔"), ("dog", "狗"), ("cat", "猫"),
        ("car", "汽车"), ("house", "房子"), ("table", "桌子"), ("chair", "椅子"),
        ("school", "学校"), ("teacher", "老师"), ("student", "学生"), ("computer", "电脑"),
        ("phone", "手机"), ("bag", "包"), ("river", "河流"), ("mountain", "山"),
        ("city", "城市"), ("country", "国家"), ("garden", "花园"), ("door", "门"),
        ("window", "窗户"), ("cup", "杯子"), ("ball", "球"), ("game", "游戏"),
        ("song", "歌"), ("movie", "电影"), ("letter", "信"), ("word", "单词"),
        ("university", "大学"), ("uniform", "制服"), ("useful", "有用的"), ("European", "欧洲的"),
        ("one-way", "单行的"), ("unit", "单元"), ("unicorn", "独角兽"), ("used", "用过的"),
    ]

    an_words = [
        ("apple", "苹果"), ("egg", "鸡蛋"), ("orange", "橙子"), ("umbrella", "雨伞"),
        ("hour", "小时"), ("honest", "诚实的"), ("honor", "荣誉"), ("heir", "继承人"),
        ("island", "岛屿"), ("elephant", "大象"), ("engineer", "工程师"), ("artist", "艺术家"),
        ("idea", "想法"), ("interview", "面试"), ("accident", "事故"), ("apartment", "公寓"),
        ("exam", "考试"), ("example", "例子"), ("exercise", "练习"), ("email", "邮件"),
        ("actor", "演员"), ("actress", "女演员"), ("ant", "蚂蚁"), ("eagle", "鹰"),
        ("error", "错误"), ("invitation", "邀请"), ("opportunity", "机会"), ("oven", "烤箱"),
        ("article", "文章"), ("opinion", "意见"), ("offer", "提议"), ("ocean", "海洋"),
    ]

    # a-word 选择题
    for word, zh in a_words:
        for ctx in [f"I saw ___ {word}.", f"She needs ___ {word}.", f"He bought ___ {word}.",
                     f"There is ___ {word} here.", f"This is ___ {word}."]:
            if word in ('university','uniform','useful','European','one-way','unit','unicorn','used'):
                hint_text = f'注意：{word} 发音以辅音音素 /j/ 或 /w/ 开头'
                diff_lvl = 'medium'
            else:
                hint_text = f'{word} 以辅音音素开头'
                diff_lvl = 'easy'
            Q.append(mc(ctx, hint_text, ["a", "an", "the", "—(无)"], 0,
                f'"{word}" 以辅音音素开头，用 a。注意：元音字母开头的词不一定用 an（如 university）。',
                [ctx.replace('___', '<strong>a</strong>')],
                f'{word} 首音是辅音 → a', 'article', diff_lvl))

    # an-word 选择题
    for word, zh in an_words:
        for ctx in [f"I saw ___ {word}.", f"She needs ___ {word}.", f"He is ___ {word}.",
                     f"There is ___ {word} here.", f"This is ___ {word}."]:
            if word in ('hour','honest','honor','heir'):
                hint_text = f'注意：{word} 中 h 不发音，以元音音素开头'
                diff_lvl = 'medium'
            else:
                hint_text = f'{word} 以元音音素开头'
                diff_lvl = 'easy'
            Q.append(mc(ctx, hint_text, ["a", "an", "the", "—(无)"], 1,
                f'"{word}" 以元音音素开头，用 an。',
                [ctx.replace('___', '<strong>an</strong>')],
                f'{word} 首音是元音 → an', 'article', diff_lvl))

    # a/an 填空题
    for word, zh in a_words[:15] + an_words[:15]:
        is_an = word in [w for w,_ in an_words]
        article = 'an' if is_an else 'a'
        diff_lvl = 'medium' if word in ('university','uniform','hour','honest','useful','European') else 'easy'
        Q.append(fl(
            f"She has ___ {word}. ({zh})",
            f'填入正确的冠词: a 或 an',
            article,
            f'"{word}" {"以元音音素开头，用 an" if is_an else "以辅音音素开头，用 a"}。',
            [f'She has <strong>{article}</strong> {word}.'],
            f'{word} → {article}', 'article', diff_lvl
        ))

    # ================================================================
    # 2. 定冠词 the 的用法
    # ================================================================
    the_contexts = [
        # (句子模板, the-短语, 不用the的短语, 解释)
        ("___ sun rises in the east.", "the sun", "a sun", "太阳是独一无二的事物，用 the"),
        ("___ earth goes around the sun.", "the earth", "an earth", "地球是独一无二的，用 the"),
        ("___ moon is very bright tonight.", "the moon", "a moon", "月亮是独一无二的，用 the"),
        ("___ sky is clear today.", "the sky", "a sky", "天空通常用 the 特指"),
        ("___ internet has changed our lives.", "the internet", "an internet", "互联网通常用 the"),
        ("___ president will give a speech.", "the president", "a president", "特指已知的总统，用 the"),
        ("___ government has made a decision.", "the government", "a government", "特指当前政府，用 the"),
        ("___ police arrived quickly.", "the police", "a police", "police 集合名词，通常加 the"),
        ("___ rich should help the poor.", "the rich", "a rich", "the + 形容词表示一类人"),
        ("___ young need good education.", "the young", "a young", "the + 形容词表示一类人"),
        ("___ first lesson is easy.", "the first", "a first", "序数词前用 the"),
        ("He is ___ best student in class.", "the best", "a best", "最高级前用 the"),
        ("___ only way is to work hard.", "the only", "an only", "only 作唯一解时用 the"),
        ("___ same thing happened again.", "the same", "a same", "same 前总是用 the"),
        ("Open ___ door, please.", "the door", "a door", "双方都知道的门，用 the"),
        ("Where is ___ bathroom?", "the bathroom", "a bathroom", "双方都知道的浴室，用 the"),
        ("I love ___ music of Mozart.", "the music", "a music", "特指莫扎特的音乐，用 the"),
        ("___ water in this river is clean.", "the water", "a water", "特指这条河的水，用 the"),
        ("___ history of China is long.", "the history", "a history", "特指中国的历史，用 the"),
        ("___ capital of France is Paris.", "the capital", "a capital", "特指法国的首都，用 the"),
    ]

    for q_text, the_phrase, no_the, explain in the_contexts:
        opts = ["the", "a", "an", "—(无)"]
        random.shuffle(opts)
        ans = opts.index("the")
        Q.append(mc(q_text, '特指/独一无二/序数词/最高级', opts, ans,
            explain,
            [q_text.replace('___', '<strong>the</strong>')],
            '独一无二/特指 → the', 'article', 'easy'))

    # ================================================================
    # 3. 零冠词（不用冠词的情况）
    # ================================================================
    zero_article_contexts = [
        ("___ children like playing.", "复数名词泛指"),
        ("___ dogs are loyal animals.", "复数名词泛指"),
        ("___ books are our best friends.", "复数名词泛指"),
        ("___ water is essential for life.", "不可数名词泛指"),
        ("___ milk is good for health.", "不可数名词泛指"),
        ("___ music makes people happy.", "不可数名词泛指"),
        ("___ knowledge is power.", "不可数名词泛指"),
        ("___ time is money.", "不可数名词泛指"),
        ("___ breakfast is ready.", "三餐前不加冠词"),
        ("We have ___ lunch at 12.", "三餐前不加冠词"),
        ("___ dinner was delicious.", "三餐前不加冠词"),
        ("___ English is a global language.", "语言名称前不加冠词"),
        ("___ Chinese is difficult to learn.", "语言名称前不加冠词"),
        ("___ math is my favorite subject.", "学科名称前不加冠词"),
        ("___ science has made great progress.", "学科名称前不加冠词"),
        ("___ basketball is popular worldwide.", "球类运动前不加冠词"),
        ("He plays ___ football every weekend.", "球类运动前不加冠词"),
        ("___ tennis is fun to play.", "球类运动前不加冠词"),
        ("___ Christmas is coming.", "节日名称前不加冠词"),
        ("___ New Year's Day is a holiday.", "节日名称前不加冠词"),
        ("___ Beijing is the capital.", "专有名词前不加冠词"),
        ("___ Mount Everest is the highest.", "山名前不加冠词"),
        ("___ Lake Baikal is very deep.", "湖名前不加冠词"),
        ("He went to ___ bed early.", "go to bed 固定搭配"),
        ("She is at ___ school now.", "at school 固定搭配"),
        ("He is in ___ hospital.", "in hospital 表示住院"),
        ("They are at ___ church.", "at church 固定搭配"),
        ("She goes to ___ work by bus.", "go to work 固定搭配"),
        ("I go to ___ school by bike.", "go to school 固定搭配"),
        ("He is in ___ prison for theft.", "in prison 固定搭配"),
    ]

    for q_text, explain in zero_article_contexts:
        opts = ["the", "a", "an", "—(无)"]
        random.shuffle(opts)
        ans = opts.index("—(无)")
        Q.append(mc(q_text, '不用冠词的场景', opts, ans,
            f'{explain}，所以不用冠词。',
            [q_text.replace('___ ', '')],
            explain, 'article', 'medium'))

    # ================================================================
    # 4. 冠词固定搭配
    # ================================================================
    fixed_expr = [
        ("in ___ hurry", "a", "in a hurry = 匆忙"),
        ("have ___ cold", "a", "have a cold = 感冒"),
        ("have ___ fever", "a", "have a fever = 发烧"),
        ("have ___ headache", "a", "have a headache = 头疼"),
        ("have ___ good time", "a", "have a good time = 玩得开心"),
        ("take ___ walk", "a", "take a walk = 散步"),
        ("take ___ rest", "a", "take a rest = 休息"),
        ("take ___ shower", "a", "take a shower = 淋浴"),
        ("make ___ mistake", "a", "make a mistake = 犯错"),
        ("make ___ decision", "a", "make a decision = 做决定"),
        ("make ___ difference", "a", "make a difference = 有影响"),
        ("make ___ living", "a", "make a living = 谋生"),
        ("tell ___ lie", "a", "tell a lie = 说谎"),
        ("tell ___ story", "a", "tell a story = 讲故事"),
        ("as ___ result", "a", "as a result = 结果"),
        ("as ___ whole", "a", "as a whole = 整体上"),
        ("at ___ loss", "a", "at a loss = 不知所措"),
        ("in ___ word", "a", "in a word = 总而言之"),
        ("in ___ way", "a", "in a way = 在某种程度上"),
        ("all of ___ sudden", "a", "all of a sudden = 突然"),
        ("once in ___ while", "a", "once in a while = 偶尔"),
        ("as ___ matter of fact", "a", "as a matter of fact = 事实上"),
        ("in ___ sense", "a", "in a sense = 从某种意义上"),
        ("at ___ time", "a", "at a time = 每次"),
        ("a couple ___", "of", "a couple of = 几个"),
        ("a lot ___", "of", "a lot of = 许多"),
        ("a number ___", "of", "a number of = 许多"),
        ("plenty ___", "of", "plenty of = 充足的"),
        ("lots ___", "of", "lots of = 许多"),
        ("a great deal ___", "of", "a great deal of = 大量的"),
        ("in ___ end", "the", "in the end = 最终"),
        ("at ___ moment", "the", "at the moment = 此刻"),
        ("at ___ same time", "the", "at the same time = 同时"),
        ("by ___ way", "the", "by the way = 顺便说"),
        ("on ___ other hand", "the", "on the other hand = 另一方面"),
        ("on ___ whole", "the", "on the whole = 大体上"),
        ("in ___ distance", "the", "in the distance = 在远处"),
        ("in ___ meantime", "the", "in the meantime = 与此同时"),
        ("for ___ time being", "the", "for the time being = 暂时"),
        ("on ___ contrary", "the", "on the contrary = 相反"),
        ("out of ___ question", "the", "out of the question = 不可能"),
        ("at ___ top of", "the", "at the top of = 在...顶部"),
        ("at ___ bottom of", "the", "at the bottom of = 在...底部"),
        ("at ___ beginning of", "the", "at the beginning of = 在...开始"),
        ("at ___ end of", "the", "at the end of = 在...结尾"),
        ("___ more ... ___ more", "the ... the", "the more...the more = 越...越..."),
        ("___ sooner ___ better", "the ... the", "the sooner the better = 越快越好"),
        ("in ___ morning", "the", "in the morning = 在早上"),
        ("in ___ afternoon", "the", "in the afternoon = 在下午"),
        ("in ___ evening", "the", "in the evening = 在晚上"),
        ("at ___ weekend", "the", "at the weekend = 在周末(英)"),
        ("go to ___ cinema", "the", "go to the cinema = 去看电影"),
        ("go to ___ theater", "the", "go to the theater = 去看戏"),
        ("listen to ___ radio", "the", "listen to the radio = 听收音机"),
        ("play ___ piano", "the", "play the piano = 弹钢琴"),
        ("play ___ guitar", "the", "play the guitar = 弹吉他"),
        ("play ___ violin", "the", "play the violin = 拉小提琴"),
        ("___ Great Wall", "the", "the Great Wall = 长城"),
        ("___ United States", "the", "the United States = 美国"),
        ("___ United Kingdom", "the", "the United Kingdom = 英国"),
        ("___ Pacific Ocean", "the", "the Pacific Ocean = 太平洋"),
    ]

    for phrase, answer, explain in fixed_expr:
        if answer in ('a', 'the', '—(无)'):
            opts = ["the", "a", "an", "—(无)"]
            random.shuffle(opts)
            ans = opts.index(answer)
            Q.append(mc(f"Complete: {phrase}", explain, opts, ans,
                explain,
                [phrase.replace('___', f'<strong>{answer}</strong>')],
                explain, 'article', 'medium'))
        elif answer == 'of':
            opts = ["of", "in", "for", "with"]
            random.shuffle(opts)
            Q.append(mc(f"Complete: {phrase}", explain, opts, opts.index('of'),
                explain,
                [phrase.replace('___', '<strong>of</strong>')],
                explain, 'article', 'easy'))
        elif '...' in answer:
            # the...the... 结构
            Q.append(fl(
                f"Complete: ___ more you practice, ___ better you get.",
                'the more...the more... 结构',
                'the ... the',
                'the more...the more... = 越...越...',
                ['<strong>The more</strong> you practice, <strong>the better</strong> you get.'],
                'the + 比较级, the + 比较级', 'article', 'hard'
            ))

    # ================================================================
    # 5. 综合辨析题 — a vs an vs the vs 零冠词
    # ================================================================
    mixed_sentences = [
        ("I need ___ pen and ___ paper.", ["a", "—(无)"], ["a", "the", "an", "—(无)"],
         "pen 可数单数用 a，paper 不可数泛指不用冠词", 'medium'),
        ("She is ___ honest person and ___ good friend.", ["an", "a"], ["a", "an", "the", "—(无)"],
         "honest 以元音开头用 an，good friend 泛指用 a", 'hard'),
        ("___ sun is ___ star.", ["the", "a"], ["the", "a", "an", "—(无)"],
         "sun 独一无二用 the，star 泛指用 a", 'medium'),
        ("He goes to ___ school by ___ bus.", ["—(无)", "—(无)"], ["the", "a", "an", "—(无)"],
         "go to school 和 by bus 都是固定搭配，不用冠词", 'medium'),
        ("I had ___ apple and ___ banana for breakfast.", ["an", "a"], ["a", "an", "the", "—(无)"],
         "apple 元音开头用 an，banana 辅音开头用 a", 'easy'),
        ("___ more I learn, ___ more I realize I don't know.", ["the", "the"], ["the", "a", "an", "—(无)"],
         "the more...the more... 结构", 'hard'),
        ("___ China is ___ largest country in Asia.", ["—(无)", "the"], ["the", "a", "an", "—(无)"],
         "国家名不用冠词，最高级用 the", 'medium'),
        ("Can you play ___ guitar? I play ___ basketball.", ["the", "—(无)"], ["the", "a", "an", "—(无)"],
         "乐器用 the，球类不用冠词", 'medium'),
        ("She is ___ university student from ___ Europe.", ["a", "—(无)"], ["a", "an", "the", "—(无)"],
         "university 辅音音素开头用 a，Europe 专有名词不用冠词", 'hard'),
        ("He was ___ first person to arrive and ___ last to leave.", ["the", "the"], ["the", "a", "an", "—(无)"],
         "序数词前用 the", 'easy'),
        ("___ milk in the fridge has gone bad. I need to buy ___ fresh milk.", ["the", "—(无)"], ["the", "a", "an", "—(无)"],
         "特指冰箱里的牛奶用 the，fresh milk 泛指不可数不用冠词", 'hard'),
        ("It's ___ honor to meet you. You are ___ very kind person.", ["an", "a"], ["a", "an", "the", "—(无)"],
         "honor 的 h 不发音用 an，kind person 泛指用 a", 'hard'),
    ]

    for q_text, answers, opts_pool, explain, diff_lvl in mixed_sentences:
        # 每个空独立出题
        blanks = [m.start() for m in __import__('re').finditer(r'___', q_text)]
        for i, ans in enumerate(answers):
            opts = opts_pool.copy()
            random.shuffle(opts)
            a_idx = opts.index(ans)
            Q.append(mc(
                q_text.replace('___', f'[{i+1}]', 1).replace(f'[{i+1}]', '___'),
                f'填入第 {i+1} 个空',
                opts, a_idx,
                explain,
                [q_text.replace('___', f'<strong>{answers[0]}</strong>', 1).replace('___', f'<strong>{answers[1]}</strong>', 1)],
                explain, 'article', diff_lvl
            ))

    # ================================================================
    # 6. 更多综合填空题
    # ================================================================
    fill_sentences = [
        ("Paris is ___ capital of France.", "the", "特指法国的首都"),
        ("I want to be ___ engineer when I grow up.", "an", "engineer 元音开头"),
        ("She has ___ very interesting idea.", "a", "very 辅音开头"),
        ("He is ___ best player on the team.", "the", "最高级用 the"),
        ("Would you like ___ cup of tea?", "a", "a cup of 固定搭配"),
        ("___ Great Wall is a wonder of the world.", "the", "专有名词 the Great Wall"),
        ("I go to ___ bed at 10 every night.", "—(无)", "go to bed 固定搭配"),
        ("___ happiness is more important than money.", "—(无)", "抽象名词泛指不加冠词"),
        ("He was sent to ___ prison for his crime.", "—(无)", "in prison 固定搭配（服刑）"),
        ("Let's go to ___ cinema tonight.", "the", "go to the cinema 固定搭配"),
        ("She plays ___ violin beautifully.", "the", "乐器前加 the"),
        ("___ elephant is the largest land animal.", "the", "the + 单数名词表示一类"),
        ("___ Nile is the longest river in Africa.", "the", "河流名前加 the"),
        ("He is ___ only child in his family.", "the", "only 前用 the"),
        ("What ___ beautiful day!", "a", "感叹句 what a + 可数名词单数"),
        ("___ rich are not always happy.", "the", "the + 形容词 = 一类人"),
        ("I have ___ headache and need to rest.", "a", "have a headache 固定搭配"),
        ("___ information you gave me was useful.", "the", "特指你给我的信息"),
        ("It's ___ pity that you can't come.", "a", "It's a pity that... 固定句型"),
        ("She is ___ most beautiful girl I've ever seen.", "the", "最高级 + 限定范围用 the"),
        ("We had ___ wonderful time at the party.", "a", "have a wonderful time"),
        ("___ Alps are covered with snow.", "the", "山脉名前加 the"),
        ("He took ___ umbrella because it was raining.", "an", "umbrella 元音开头"),
        ("___ breakfast is the most important meal.", "—(无)", "三餐泛指不加冠词"),
        ("It took me ___ hour to finish the work.", "an", "hour 的 h 不发音"),
        ("He is learning to play ___ chess.", "—(无)", "棋类运动不加冠词"),
        ("___ love is a beautiful thing.", "—(无)", "抽象名词泛指不加冠词"),
        ("We need to buy ___ new furniture.", "—(无)", "furniture 不可数，不加 a/an"),
        ("___ advice he gave was very helpful.", "the", "特指他给的建议"),
        ("She works as ___ nurse in a hospital.", "a", "as a + 职业"),
    ]

    for q_text, ans, explain in fill_sentences:
        Q.append(fl(q_text, explain, ans, explain,
            [q_text.replace('___', f'<strong>{ans if ans != "—(无)" else ""}</strong>')],
            explain, 'article', 'medium' if ans == '—(无)' else 'easy'))

    return Q

# 生成并保存
new_qs = gen_articles()
all_qs = existing + new_qs

# 去重（基于 q 文本）
seen = set()
unique = []
for q in all_qs:
    key = q['q']
    if key not in seen:
        seen.add(key)
        unique.append(q)
    # 否则跳过重复

print(f"原有: {len(existing)} 题")
print(f"新增: {len(new_qs)} 题")
print(f"去重后: {len(unique)} 题")

with open(existing_file, 'w', encoding='utf-8') as f:
    json.dump(unique, f, ensure_ascii=False, indent=2)

print(f"✅ articles.json: {len(unique)} 题")
