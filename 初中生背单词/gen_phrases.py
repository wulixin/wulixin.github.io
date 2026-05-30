#!/usr/bin/env python3
"""短语和词语搭配题库生成器"""

import json
import random
import os

random.seed(42)

OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ============================================================
# 素材库
# ============================================================

# ---------- 动词短语素材 ----------
VP_DATA = {
    "give up": {
        "meaning": "放弃",
        "explain": "give(给) + up(向上→放手) → 放弃某事/习惯",
        "examples": [
            "Never give up your dream. 永远不要放弃你的梦想。",
            "He gave up smoking last year. 他去年戒烟了。"
        ],
        "mnemonic": "给出去(up)就放弃"
    },
    "give away": {
        "meaning": "赠送；泄露",
        "explain": "give(给) + away(离开) → 把东西给出去 → 赠送/泄露秘密",
        "examples": [
            "She gave away her old clothes. 她捐出了旧衣服。",
            "Don't give away the surprise! 别泄露惊喜！"
        ],
        "mnemonic": "给走(away)就赠送"
    },
    "give in": {
        "meaning": "屈服；让步",
        "explain": "give(给) + in(进入) → 把自己交进去 → 屈服",
        "examples": [
            "He refused to give in to pressure. 他拒绝向压力屈服。",
            "After hours of argument, she finally gave in. 争论几小时后，她终于让步了。"
        ],
        "mnemonic": "给进(in)去就屈服"
    },
    "give out": {
        "meaning": "分发；耗尽",
        "explain": "give(给) + out(出去) → 把东西分发出去 / 精力用完",
        "examples": [
            "The teacher gave out the test papers. 老师分发了试卷。",
            "Her patience finally gave out. 她终于失去了耐心。"
        ],
        "mnemonic": "给出(out)去分发，耗尽了也分发"
    },
    "take off": {
        "meaning": "起飞；脱下；成功",
        "explain": "take(拿) + off(离开) → 拿掉→脱下 / 飞机离开地面→起飞",
        "examples": [
            "The plane took off at 8 a.m. 飞机早上八点起飞。",
            "His business really took off. 他的生意真的大获成功。"
        ],
        "mnemonic": "拿走(off)就起飞"
    },
    "take over": {
        "meaning": "接管；接替",
        "explain": "take(拿) + over(在上方) → 拿过来管→接管",
        "examples": [
            "She took over the company after her father retired. 父亲退休后她接管了公司。",
            "Who will take over your job? 谁会接替你的工作？"
        ],
        "mnemonic": "拿到上面(over)就接管"
    },
    "take up": {
        "meaning": "开始从事；占据",
        "explain": "take(拿) + up(向上) → 拿起来开始做 → 开始从事",
        "examples": [
            "I want to take up painting. 我想开始学画画。",
            "This desk takes up too much space. 这张桌子占了太多空间。"
        ],
        "mnemonic": "拿起来(up)就开始"
    },
    "take on": {
        "meaning": "承担；呈现",
        "explain": "take(拿) + on(在...上) → 把责任担在肩上→承担",
        "examples": [
            "She decided to take on the challenge. 她决定接受挑战。",
            "The city took on a new look. 城市焕然一新。"
        ],
        "mnemonic": "担在(on)肩上就承担"
    },
    "turn down": {
        "meaning": "拒绝；调低",
        "explain": "turn(转) + down(向下) → 把音量转下去 / 把请求转回去",
        "examples": [
            "He turned down the job offer. 他拒绝了工作邀请。",
            "Please turn down the music. 请把音乐关小。"
        ],
        "mnemonic": "转下去(down)就拒绝"
    },
    "turn up": {
        "meaning": "出现；调高",
        "explain": "turn(转) + up(向上) → 音量调上去 / 人突然出现",
        "examples": [
            "She didn't turn up for the meeting. 她没来开会。",
            "Turn up the heat, it's cold. 把暖气开大，很冷。"
        ],
        "mnemonic": "转上来(up)就出现"
    },
    "turn on": {
        "meaning": "打开；取决于",
        "explain": "turn(转) + on(开) → 旋转打开开关",
        "examples": [
            "Turn on the light, please. 请开灯。",
            "The success turns on good planning. 成功取决于良好的规划。"
        ],
        "mnemonic": "转开(on)就打开"
    },
    "turn off": {
        "meaning": "关掉；使厌烦",
        "explain": "turn(转) + off(关) → 旋转关闭开关",
        "examples": [
            "Don't forget to turn off the TV. 别忘了关电视。",
            "His rude behavior really turned me off. 他的粗鲁行为让我很反感。"
        ],
        "mnemonic": "转关(off)就关闭"
    },
    "turn out": {
        "meaning": "结果是；生产",
        "explain": "turn(转) + out(出来) → 最终转出来的结果",
        "examples": [
            "It turned out to be a great day. 结果是个美好的一天。",
            "The factory turns out 1000 cars a day. 工厂每天生产1000辆车。"
        ],
        "mnemonic": "转出来(out)的结果"
    },
    "put off": {
        "meaning": "推迟；使反感",
        "explain": "put(放) + off(离开) → 把事情推开→推迟",
        "examples": [
            "Don't put off your homework. 别拖延作业。",
            "The smell put me off my food. 气味让我吃不下饭。"
        ],
        "mnemonic": "放走(off)就推迟"
    },
    "put on": {
        "meaning": "穿上；上演；增加",
        "explain": "put(放) + on(在...上) → 把衣服放在身上→穿上",
        "examples": [
            "Put on your coat, it's cold. 穿上外套，天冷。",
            "They put on a great show. 他们上演了一场精彩的表演。"
        ],
        "mnemonic": "放上(on)身就穿上"
    },
    "put up": {
        "meaning": "搭建；张贴；忍受",
        "explain": "put(放) + up(向上) → 把东西竖起来→搭建",
        "examples": [
            "They put up a tent by the lake. 他们在湖边搭了帐篷。",
            "I can't put up with the noise. 我受不了这噪音。"
        ],
        "mnemonic": "放起来(up)就搭建"
    },
    "put out": {
        "meaning": "熄灭；出版；麻烦",
        "explain": "put(放) + out(出去) → 把火放出去→熄灭",
        "examples": [
            "Firefighters put out the fire quickly. 消防员很快扑灭了大火。",
            "I hope I'm not putting you out. 希望没给你添麻烦。"
        ],
        "mnemonic": "放出去(out)就熄灭"
    },
    "put forward": {
        "meaning": "提出；推荐",
        "explain": "put(放) + forward(向前) → 把想法放到前面→提出",
        "examples": [
            "She put forward a good suggestion. 她提出了一个好建议。",
            "He was put forward for the award. 他被推荐获奖。"
        ],
        "mnemonic": "放向前(forward)就提出"
    },
    "get up": {
        "meaning": "起床；站起",
        "explain": "get(得到) + up(向上) → 从床上起来",
        "examples": [
            "I get up at 6 every morning. 我每天早上六点起床。",
            "He got up from his chair. 他从椅子上站起来。"
        ],
        "mnemonic": "起来(up)就起床"
    },
    "get along": {
        "meaning": "相处；进展",
        "explain": "get(变得) + along(沿着) → 沿着一起走→相处融洽",
        "examples": [
            "Do you get along with your classmates? 你和同学相处得好吗？",
            "How are you getting along with your work? 你工作进展如何？"
        ],
        "mnemonic": "一起走(along)就相处"
    },
    "get over": {
        "meaning": "克服；从...恢复",
        "explain": "get(变得) + over(越过) → 越过困难→克服",
        "examples": [
            "It took her months to get over the illness. 她花了几个月才康复。",
            "You need to get over your fear. 你需要克服恐惧。"
        ],
        "mnemonic": "越过去(over)就克服"
    },
    "get through": {
        "meaning": "完成；通过；熬过",
        "explain": "get(变得) + through(穿过) → 穿过困难→熬过",
        "examples": [
            "I finally got through the exam. 我终于通过了考试。",
            "We'll get through this together. 我们一起熬过去。"
        ],
        "mnemonic": "穿过去(through)就完成"
    },
    "get by": {
        "meaning": "勉强过活；应付",
        "explain": "get(得到) + by(旁边) → 从旁边勉强过去→凑合过",
        "examples": [
            "We can get by with a small budget. 预算少我们也能凑合。",
            "He doesn't earn much but he gets by. 他赚得不多但还过得去。"
        ],
        "mnemonic": "旁边(by)过就凑合"
    },
    "look for": {
        "meaning": "寻找",
        "explain": "look(看) + for(为了) → 为了找到而看→寻找",
        "examples": [
            "I'm looking for my keys. 我在找钥匙。",
            "What are you looking for? 你在找什么？"
        ],
        "mnemonic": "看为了(for)找东西"
    },
    "look after": {
        "meaning": "照顾",
        "explain": "look(看) + after(在...之后) → 在身后看着→照顾",
        "examples": [
            "She looks after her little brother. 她照顾弟弟。",
            "Who will look after the garden? 谁来照看花园？"
        ],
        "mnemonic": "在后面(after)看就照顾"
    },
    "look into": {
        "meaning": "调查；研究",
        "explain": "look(看) + into(进入) → 往里面看→调查",
        "examples": [
            "The police are looking into the case. 警方正在调查此案。",
            "Let me look into it. 让我研究一下。"
        ],
        "mnemonic": "往里(into)看就调查"
    },
    "look up": {
        "meaning": "查阅；仰望",
        "explain": "look(看) + up(向上) → 抬头看/在书中查找",
        "examples": [
            "Look up the word in the dictionary. 在词典里查这个词。",
            "Things are looking up! 情况在好转！"
        ],
        "mnemonic": "向上(up)看就查阅"
    },
    "look forward to": {
        "meaning": "期待",
        "explain": "look(看) + forward(向前) + to(朝向) → 向前看→期待",
        "examples": [
            "I look forward to hearing from you. 我期待你的回复。",
            "We're looking forward to the summer vacation. 我们期待暑假。"
        ],
        "mnemonic": "向前(forward)看就期待"
    },
    "look down on": {
        "meaning": "看不起",
        "explain": "look(看) + down(向下) + on → 往下看别人→看不起",
        "examples": [
            "Don't look down on the poor. 不要看不起穷人。",
            "He looks down on people without education. 他看不起没受过教育的人。"
        ],
        "mnemonic": "往下(down)看就瞧不起"
    },
    "bring up": {
        "meaning": "抚养；提出",
        "explain": "bring(带来) + up(向上) → 把孩子带大→抚养",
        "examples": [
            "She was brought up by her grandparents. 她是祖父母养大的。",
            "He brought up an interesting point. 他提出了一个有趣的观点。"
        ],
        "mnemonic": "带大(up)就抚养"
    },
    "bring about": {
        "meaning": "引起；导致",
        "explain": "bring(带来) + about(关于) → 带来某种结果→引起",
        "examples": [
            "Technology has brought about great changes. 科技带来了巨大变化。",
            "What brought about the accident? 事故是怎么引起的？"
        ],
        "mnemonic": "带来(about)变化就引起"
    },
    "bring out": {
        "meaning": "出版；使显现",
        "explain": "bring(带来) + out(出来) → 把东西带出来→出版",
        "examples": [
            "The company brought out a new product. 公司推出了新产品。",
            "The sun brought out her freckles. 太阳让她的雀斑显现出来。"
        ],
        "mnemonic": "带出来(out)就出版"
    },
    "call off": {
        "meaning": "取消",
        "explain": "call(叫) + off(离开) → 把安排叫走→取消",
        "examples": [
            "They called off the meeting. 他们取消了会议。",
            "The game was called off due to rain. 比赛因雨取消。"
        ],
        "mnemonic": "叫走(off)就取消"
    },
    "call on": {
        "meaning": "拜访；号召",
        "explain": "call(叫) + on(在...上) → 叫上某人→拜访/号召",
        "examples": [
            "I'll call on you tomorrow. 我明天拜访你。",
            "The teacher called on me to answer. 老师叫我回答问题。"
        ],
        "mnemonic": "叫上(on)门就拜访"
    },
    "call for": {
        "meaning": "需要；要求",
        "explain": "call(叫) + for(为了) → 为某事而呼喊→要求",
        "examples": [
            "This situation calls for immediate action. 这种情况需要立即行动。",
            "The workers called for higher wages. 工人要求涨工资。"
        ],
        "mnemonic": "叫为了(for)就需要"
    },
    "carry out": {
        "meaning": "执行；实施",
        "explain": "carry(搬运) + out(出去) → 把计划搬出去执行",
        "examples": [
            "We need to carry out the plan. 我们需要执行计划。",
            "The experiment was carried out carefully. 实验被认真执行了。"
        ],
        "mnemonic": "搬出去(out)就执行"
    },
    "carry on": {
        "meaning": "继续；进行",
        "explain": "carry(搬运) + on(继续) → 继续搬运→继续进行",
        "examples": [
            "Carry on with your work. 继续你的工作。",
            "They carried on talking all night. 他们聊了一整晚。"
        ],
        "mnemonic": "搬继续(on)就继续"
    },
    "come across": {
        "meaning": "偶然遇见；发现",
        "explain": "come(来) + across(横过) → 横跨过来时遇到→偶遇",
        "examples": [
            "I came across an old photo. 我偶然发现了一张老照片。",
            "She came across as very confident. 她给人很自信的印象。"
        ],
        "mnemonic": "横过来(across)就偶遇"
    },
    "come up with": {
        "meaning": "想出；提出",
        "explain": "come(来) + up(向上) + with → 想法从脑子里冒出来→想出",
        "examples": [
            "He came up with a great idea. 他想出了一个好主意。",
            "Can you come up with a solution? 你能想出解决方案吗？"
        ],
        "mnemonic": "冒上来(up with)就想到"
    },
    "cut down": {
        "meaning": "削减；砍倒",
        "explain": "cut(切) + down(向下) → 把数量切下去→削减",
        "examples": [
            "We need to cut down on expenses. 我们需要削减开支。",
            "They cut down the old tree. 他们砍倒了老树。"
        ],
        "mnemonic": "切下去(down)就削减"
    },
    "cut off": {
        "meaning": "切断；中断",
        "explain": "cut(切) + off(离开) → 切掉分离→切断",
        "examples": [
            "The phone call was cut off. 电话被切断了。",
            "They cut off the water supply. 他们切断了供水。"
        ],
        "mnemonic": "切掉(off)就切断"
    },
    "drop out": {
        "meaning": "辍学；退出",
        "explain": "drop(掉下) + out(出来) → 从学校/活动中掉出来→退出",
        "examples": [
            "He dropped out of school at 16. 他16岁辍学。",
            "She dropped out of the race. 她退出了比赛。"
        ],
        "mnemonic": "掉出来(out)就退出"
    },
    "drop by": {
        "meaning": "顺便拜访",
        "explain": "drop(掉下) + by(旁边) → 路过顺便落下→顺便拜访",
        "examples": [
            "Feel free to drop by anytime. 随时欢迎来坐坐。",
            "I'll drop by your office later. 我待会儿去你办公室坐坐。"
        ],
        "mnemonic": "旁边(by)落下就顺便"
    },
    "figure out": {
        "meaning": "弄明白；算出",
        "explain": "figure(数字/形状) + out(出来) → 把形状弄出来→弄清楚",
        "examples": [
            "I can't figure out this math problem. 我解不出这道数学题。",
            "Have you figured out what happened? 你搞清楚发生什么了吗？"
        ],
        "mnemonic": "算出来(out)就明白"
    },
    "find out": {
        "meaning": "发现；查明",
        "explain": "find(找到) + out(出来) → 把真相找出来→发现",
        "examples": [
            "How did you find out? 你怎么发现的？",
            "Let's find out the truth. 我们来查明真相。"
        ],
        "mnemonic": "找出来(out)就发现"
    },
    "go on": {
        "meaning": "继续；发生",
        "explain": "go(走) + on(继续) → 继续走→继续进行",
        "examples": [
            "Please go on with your story. 请继续你的故事。",
            "What's going on here? 这里发生什么事了？"
        ],
        "mnemonic": "走继续(on)就继续"
    },
    "go over": {
        "meaning": "复习；仔细检查",
        "explain": "go(走) + over(在上面) → 在内容上走一遍→复习",
        "examples": [
            "Let's go over the lesson again. 我们再复习一遍课文。",
            "I need to go over these numbers. 我需要仔细核对这些数字。"
        ],
        "mnemonic": "走一遍(over)就复习"
    },
    "go through": {
        "meaning": "经历；仔细检查",
        "explain": "go(走) + through(穿过) → 穿过困难→经历",
        "examples": [
            "She has gone through a lot. 她经历了很多。",
            "Let's go through the document together. 我们一起过一遍文件。"
        ],
        "mnemonic": "穿过去(through)就经历"
    },
    "hold on": {
        "meaning": "坚持；稍等",
        "explain": "hold(握) + on(继续) → 继续握住→坚持/别挂电话",
        "examples": [
            "Hold on, I'll be right back. 稍等，我马上回来。",
            "Hold on to your dreams. 坚持你的梦想。"
        ],
        "mnemonic": "握着(on)就坚持"
    },
    "hold back": {
        "meaning": "抑制；阻止",
        "explain": "hold(握) + back(向后) → 往后拉→阻止",
        "examples": [
            "She couldn't hold back her tears. 她忍不住哭了。",
            "Nothing can hold back progress. 没有什么能阻止进步。"
        ],
        "mnemonic": "往后(back)拉就阻止"
    },
    "keep up": {
        "meaning": "保持；跟上",
        "explain": "keep(保持) + up(向上) → 保持向上→跟上",
        "examples": [
            "Keep up the good work! 保持好工作！",
            "I can't keep up with the new technology. 我跟不上新技术。"
        ],
        "mnemonic": "保持上(up)去就跟上"
    },
    "keep on": {
        "meaning": "继续",
        "explain": "keep(保持) + on(继续) → 继续保持→持续",
        "examples": [
            "Keep on trying, you'll succeed. 继续努力，你会成功的。",
            "The rain kept on all day. 雨下了一整天。"
        ],
        "mnemonic": "保持继续(on)就继续"
    },
    "let down": {
        "meaning": "让...失望",
        "explain": "let(让) + down(向下) → 让人心情低落→失望",
        "examples": [
            "I won't let you down. 我不会让你失望。",
            "He felt let down by his friend. 他对朋友感到失望。"
        ],
        "mnemonic": "让下去(down)就失望"
    },
    "let in": {
        "meaning": "让...进入",
        "explain": "let(让) + in(进入) → 允许进入",
        "examples": [
            "Please let me in, it's cold outside. 让我进去，外面冷。",
            "The window lets in fresh air. 窗户让新鲜空气进来。"
        ],
        "mnemonic": "让进(in)来就进入"
    },
    "make up": {
        "meaning": "编造；弥补；化妆",
        "explain": "make(做) + up(向上) → 把故事做出来→编造 / 把分数补上→弥补",
        "examples": [
            "He made up an excuse. 他编了一个借口。",
            "We need to make up for lost time. 我们需要弥补失去的时间。"
        ],
        "mnemonic": "做起来(up)就编造/弥补"
    },
    "make out": {
        "meaning": "辨认出；理解",
        "explain": "make(做) + out(出来) → 从模糊中做出来→辨认",
        "examples": [
            "I can't make out what he's saying. 我听不清他在说什么。",
            "Can you make out the sign in the distance? 你能看清远处的标志吗？"
        ],
        "mnemonic": "做出来(out)就辨认"
    },
    "pass away": {
        "meaning": "去世",
        "explain": "pass(经过) + away(离开) → 生命离开→去世",
        "examples": [
            "His grandfather passed away last year. 他祖父去年去世了。",
            "She passed away peacefully in her sleep. 她在睡梦中安详离世。"
        ],
        "mnemonic": "走过去(away)就去世"
    },
    "pass out": {
        "meaning": "昏倒；分发",
        "explain": "pass(经过) + out(出去) → 意识出去了→昏倒",
        "examples": [
            "He passed out from the heat. 他中暑昏倒了。",
            "They passed out flyers on the street. 他们在街上发传单。"
        ],
        "mnemonic": "过去(out)了就昏倒"
    },
    "pick up": {
        "meaning": "捡起；接人；学会",
        "explain": "pick(捡) + up(向上) → 把东西捡起来",
        "examples": [
            "Can you pick me up at the station? 你能来车站接我吗？",
            "She picked up Spanish very quickly. 她很快学会了西班牙语。"
        ],
        "mnemonic": "捡起来(up)就学会"
    },
    "pick out": {
        "meaning": "挑选；辨认出",
        "explain": "pick(挑选) + out(出来) → 从众多中挑出来",
        "examples": [
            "Can you pick out the right one? 你能挑出正确的吗？",
            "She was easy to pick out in the crowd. 她在人群中很容易辨认。"
        ],
        "mnemonic": "挑出来(out)就辨认"
    },
    "point out": {
        "meaning": "指出",
        "explain": "point(指) + out(出来) → 用手指出来→指出",
        "examples": [
            "He pointed out my mistake. 他指出了我的错误。",
            "I should point out that this is important. 我应该指出这很重要。"
        ],
        "mnemonic": "指出来(out)就指出"
    },
    "pull over": {
        "meaning": "靠边停车",
        "explain": "pull(拉) + over(到一边) → 把车拉到路边→靠边停车",
        "examples": [
            "The police asked him to pull over. 警察让他靠边停车。",
            "Pull over here, I see a parking spot. 靠边停，我看到车位了。"
        ],
        "mnemonic": "拉过去(over)就靠边"
    },
    "pull through": {
        "meaning": "渡过难关；康复",
        "explain": "pull(拉) + through(穿过) → 把病人/困境拉过来→渡过",
        "examples": [
            "The doctors say he will pull through. 医生说他能康复。",
            "We'll pull through this crisis. 我们会渡过这个危机。"
        ],
        "mnemonic": "拉过来(through)就渡过"
    },
    "run into": {
        "meaning": "偶遇；撞上",
        "explain": "run(跑) + into(进入) → 跑着跑着撞进某人→偶遇",
        "examples": [
            "I ran into an old friend yesterday. 昨天我偶遇了一位老朋友。",
            "We've run into some problems. 我们遇到了一些问题。"
        ],
        "mnemonic": "跑进(into)去就偶遇"
    },
    "run out of": {
        "meaning": "用完；耗尽",
        "explain": "run(跑) + out of(从...出去) → 东西跑光了→用完",
        "examples": [
            "We're running out of time. 我们没时间了。",
            "I've run out of milk. 我的牛奶用完了。"
        ],
        "mnemonic": "跑光了(out of)就用完"
    },
    "run over": {
        "meaning": "撞倒；溢出；过一遍",
        "explain": "run(跑) + over(在上面) → 车从上面压过去→撞倒",
        "examples": [
            "He was nearly run over by a car. 他差点被车撞了。",
            "Let's run over the main points again. 我们再过一遍要点。"
        ],
        "mnemonic": "跑过去(over)就撞倒"
    },
    "set up": {
        "meaning": "建立；设置",
        "explain": "set(放置) + up(向上) → 把东西竖起来→建立",
        "examples": [
            "They set up a new company. 他们成立了一家新公司。",
            "Can you help me set up the computer? 你能帮我设置电脑吗？"
        ],
        "mnemonic": "竖起来(up)就建立"
    },
    "set off": {
        "meaning": "出发；引爆；引发",
        "explain": "set(放置) + off(离开) → 放离起点→出发",
        "examples": [
            "We set off early in the morning. 我们一大早就出发了。",
            "The news set off a wave of panic. 消息引发了一阵恐慌。"
        ],
        "mnemonic": "放离(off)就出发"
    },
    "show off": {
        "meaning": "炫耀",
        "explain": "show(展示) + off(离开) → 展示给外人看→炫耀",
        "examples": [
            "He likes to show off his new car. 他喜欢炫耀新车。",
            "Stop showing off! 别炫耀了！"
        ],
        "mnemonic": "展示出去(off)就炫耀"
    },
    "show up": {
        "meaning": "出现；露面",
        "explain": "show(展示) + up(向上) → 人出现在众人面前→露面",
        "examples": [
            "She didn't show up for class. 她没来上课。",
            "The problem showed up again. 问题又出现了。"
        ],
        "mnemonic": "展示出来(up)就出现"
    },
    "stand out": {
        "meaning": "突出；显眼",
        "explain": "stand(站) + out(出来) → 从人群中站出来→突出",
        "examples": [
            "His talent makes him stand out. 他的才华让他脱颖而出。",
            "The red sign stands out clearly. 红色标志非常显眼。"
        ],
        "mnemonic": "站出来(out)就突出"
    },
    "stand for": {
        "meaning": "代表；支持",
        "explain": "stand(站) + for(为了) → 为某事物站着→代表/支持",
        "examples": [
            "What does UN stand for? UN代表什么？",
            "We stand for equality. 我们支持平等。"
        ],
        "mnemonic": "站着为了(for)就代表"
    },
    "break down": {
        "meaning": "出故障；分解；崩溃",
        "explain": "break(打破) + down(向下) → 机器/情绪被打碎→故障/崩溃",
        "examples": [
            "My car broke down on the highway. 我的车在高速上抛锚了。",
            "She broke down in tears. 她哭崩溃了。"
        ],
        "mnemonic": "打碎下去(down)就坏了"
    },
    "break up": {
        "meaning": "分手；解散",
        "explain": "break(打破) + up(向上) → 关系被打破→分手",
        "examples": [
            "They broke up after five years. 他们五年后分手了。",
            "The meeting broke up at noon. 会议中午解散。"
        ],
        "mnemonic": "打破(up)了就分手"
    },
    "break out": {
        "meaning": "爆发；逃脱",
        "explain": "break(打破) + out(出来) → 从里面打破出来→爆发",
        "examples": [
            "A fire broke out in the building. 大楼里发生了火灾。",
            "Three prisoners broke out of jail. 三个囚犯越狱了。"
        ],
        "mnemonic": "打破出来(out)就爆发"
    },
    "break into": {
        "meaning": "闯入；打断",
        "explain": "break(打破) + into(进入) → 打破门进去→闯入",
        "examples": [
            "Someone broke into my house. 有人闯入了我家。",
            "He broke into a smile. 他突然笑了起来。"
        ],
        "mnemonic": "打破进去(into)就闯入"
    },
    "work out": {
        "meaning": "锻炼；算出；解决",
        "explain": "work(工作) + out(出来) → 把问题做出来→解决 / 把身体练出来→锻炼",
        "examples": [
            "I work out at the gym every day. 我每天在健身房锻炼。",
            "We need to work out a solution. 我们需要想出一个解决方案。"
        ],
        "mnemonic": "做出来(out)就解决"
    },
    "work on": {
        "meaning": "致力于；从事",
        "explain": "work(工作) + on(在...上) → 在某事上工作→致力于",
        "examples": [
            "I'm working on a new project. 我在做一个新项目。",
            "You need to work on your pronunciation. 你需要练习发音。"
        ],
        "mnemonic": "工作在上(on)面就致力于"
    },
    "check in": {
        "meaning": "登记入住；报到",
        "explain": "check(检查) + in(进入) → 进入时登记→入住",
        "examples": [
            "We checked in at the hotel at 3 p.m. 我们下午三点在酒店办理入住。",
            "Please check in at the front desk. 请在前台报到。"
        ],
        "mnemonic": "检查进(in)去就入住"
    },
    "check out": {
        "meaning": "退房；查看",
        "explain": "check(检查) + out(出去) → 离开时检查→退房",
        "examples": [
            "We need to check out by noon. 我们需要中午前退房。",
            "Check out this cool website! 看看这个酷网站！"
        ],
        "mnemonic": "检查出(out)去就退房"
    },
    "fill in": {
        "meaning": "填写；临时替代",
        "explain": "fill(填) + in(进入) → 把内容填入→填写",
        "examples": [
            "Please fill in the form. 请填写表格。",
            "Can you fill in for me while I'm away? 我不在时你能替我一下吗？"
        ],
        "mnemonic": "填进(in)去就填写"
    },
    "fill out": {
        "meaning": "填写（表格）",
        "explain": "fill(填) + out(完全) → 把表格填完整→填写",
        "examples": [
            "Fill out this application form. 填写这份申请表。",
            "The form needs to be filled out completely. 表格需要填写完整。"
        ],
        "mnemonic": "填完(out)整就填写"
    },
    "hang out": {
        "meaning": "闲逛；消磨时间",
        "explain": "hang(悬挂) + out(出去) → 在外面挂着→闲逛",
        "examples": [
            "We like to hang out at the mall. 我们喜欢在商场闲逛。",
            "Do you want to hang out this weekend? 这周末想一起玩吗？"
        ],
        "mnemonic": "挂在外面(out)就闲逛"
    },
    "hang up": {
        "meaning": "挂断电话",
        "explain": "hang(悬挂) + up(向上) → 把电话挂上去→挂断",
        "examples": [
            "Don't hang up! I have more to say. 别挂！我还有话说。",
            "He hung up on me! 他挂我电话！"
        ],
        "mnemonic": "挂上去(up)就挂断"
    },
    "pay off": {
        "meaning": "还清；取得成功",
        "explain": "pay(支付) + off(完全) → 把钱全部付完→还清 / 努力得到回报",
        "examples": [
            "He finally paid off all his debts. 他终于还清了所有债务。",
            "All your hard work will pay off. 你的努力都会得到回报。"
        ],
        "mnemonic": "付完(off)就回报"
    },
    "pay back": {
        "meaning": "偿还；报复",
        "explain": "pay(支付) + back(返回) → 把钱还回去→偿还",
        "examples": [
            "I'll pay you back next week. 我下周还你钱。",
            "He wanted to pay them back for the insult. 他想为侮辱报复他们。"
        ],
        "mnemonic": "付回去(back)就偿还"
    },
    "rule out": {
        "meaning": "排除",
        "explain": "rule(统治/划线) + out(出去) → 划线把选项划掉→排除",
        "examples": [
            "The police ruled out suicide. 警方排除了自杀的可能。",
            "We can't rule out any possibility. 我们不能排除任何可能。"
        ],
        "mnemonic": "划掉(out)就排除"
    },
    "settle down": {
        "meaning": "安定下来；定居",
        "explain": "settle(安放) + down(向下) → 身心沉下来→安定",
        "examples": [
            "It's time to settle down and start a family. 该安定下来组建家庭了。",
            "Settle down, class! 安静，同学们！"
        ],
        "mnemonic": "沉下来(down)就安定"
    },
    "slow down": {
        "meaning": "减速；放慢",
        "explain": "slow(慢) + down(向下) → 速度降下来→减速",
        "examples": [
            "Slow down, you're driving too fast. 慢点开，你开太快了。",
            "You need to slow down and relax. 你需要慢下来放松。"
        ],
        "mnemonic": "慢下来(down)就减速"
    },
    "speed up": {
        "meaning": "加速",
        "explain": "speed(速度) + up(向上) → 速度提上来→加速",
        "examples": [
            "We need to speed up the process. 我们需要加快进程。",
            "The car sped up on the highway. 车在高速上加速了。"
        ],
        "mnemonic": "速度上去(up)就加速"
    },
    "think over": {
        "meaning": "仔细考虑",
        "explain": "think(想) + over(在上面) → 在问题上反复想→仔细考虑",
        "examples": [
            "Think it over before you decide. 决定前再想想。",
            "I need some time to think over your offer. 我需要时间考虑你的提议。"
        ],
        "mnemonic": "想一遍(over)就考虑"
    },
    "throw away": {
        "meaning": "扔掉；浪费",
        "explain": "throw(扔) + away(离开) → 把东西扔走→扔掉",
        "examples": [
            "Don't throw away those old books. 别扔掉那些旧书。",
            "Don't throw away your chance. 别浪费你的机会。"
        ],
        "mnemonic": "扔走(away)就扔掉"
    },
    "try on": {
        "meaning": "试穿；试戴",
        "explain": "try(尝试) + on(穿上) → 试穿在身上→试穿",
        "examples": [
            "Can I try on this dress? 我能试穿这件裙子吗？",
            "Try on the shoes before buying. 买之前试试鞋。"
        ],
        "mnemonic": "试上(on)身就试穿"
    },
    "try out": {
        "meaning": "试验；尝试",
        "explain": "try(尝试) + out(出来) → 把方法试出来→试验",
        "examples": [
            "Let's try out the new software. 我们试试新软件。",
            "He tried out for the basketball team. 他去试了篮球队选拔。"
        ],
        "mnemonic": "试出来(out)就试验"
    },
    "wake up": {
        "meaning": "醒来；叫醒",
        "explain": "wake(醒) + up(向上) → 从睡眠中醒来",
        "examples": [
            "I wake up at 6 every morning. 我每天早上六点醒来。",
            "Wake me up at seven. 七点叫醒我。"
        ],
        "mnemonic": "醒过来(up)就醒来"
    },
    "watch out": {
        "meaning": "小心；当心",
        "explain": "watch(看) + out(出去) → 向外看→当心危险",
        "examples": [
            "Watch out for cars! 小心车！",
            "Watch out, the floor is wet. 当心，地板湿的。"
        ],
        "mnemonic": "看出去(out)就小心"
    },
    "wear out": {
        "meaning": "磨损；使疲惫",
        "explain": "wear(穿) + out(出去) → 穿到外面都破了→磨损",
        "examples": [
            "My shoes are worn out. 我的鞋穿破了。",
            "The long journey wore me out. 长途旅行让我筋疲力尽。"
        ],
        "mnemonic": "穿出去(out)就磨损"
    },
    "take care of": {
        "meaning": "照顾；处理",
        "explain": "take(拿) + care(关心) + of → 拿关心去对待→照顾",
        "examples": [
            "She takes care of her sick mother. 她照顾生病的母亲。",
            "I'll take care of the problem. 我会处理这个问题。"
        ],
        "mnemonic": "拿关心(care)去照顾"
    },
    "take part in": {
        "meaning": "参加；参与",
        "explain": "take(拿) + part(部分) + in → 拿一部分加入→参加",
        "examples": [
            "Many students took part in the contest. 很多学生参加了比赛。",
            "Will you take part in the discussion? 你会参与讨论吗？"
        ],
        "mnemonic": "拿一份(part)去参加"
    },
    "take place": {
        "meaning": "发生；举行",
        "explain": "take(拿) + place(地方) → 拿地方来举行→发生",
        "examples": [
            "The concert will take place next Friday. 音乐会下周五举行。",
            "Great changes have taken place. 发生了巨大变化。"
        ],
        "mnemonic": "拿地方(place)举行"
    },
    "take advantage of": {
        "meaning": "利用",
        "explain": "take(拿) + advantage(优势) + of → 拿优势来用→利用",
        "examples": [
            "Take advantage of the good weather. 利用好天气。",
            "Don't take advantage of others. 不要利用别人。"
        ],
        "mnemonic": "拿优势(advantage)去利用"
    },
    "take into account": {
        "meaning": "考虑到；顾及",
        "explain": "take(拿) + into(进入) + account(账户/考虑) → 拿进考虑中→考虑到",
        "examples": [
            "We must take all factors into account. 我们必须考虑所有因素。",
            "His age should be taken into account. 应该考虑到他的年龄。"
        ],
        "mnemonic": "拿进(into)账户(account)考虑"
    },
}

# ---------- 形容词+介词搭配素材 ----------
ADJ_PREP = {
    "afraid of": {"meaning": "害怕", "examples": ["Are you afraid of the dark? 你怕黑吗？", "She is afraid of making mistakes. 她害怕犯错。"], "mnemonic": "怕(of)什么"},
    "angry with": {"meaning": "对...生气", "examples": ["My mom is angry with me. 妈妈生我的气。", "Don't be angry with yourself. 别生自己的气。"], "mnemonic": "跟(with)谁生气"},
    "anxious about": {"meaning": "对...焦虑", "examples": ["She's anxious about the exam. 她对考试很焦虑。", "Parents are anxious about their children. 父母为子女焦虑。"], "mnemonic": "关于(about)什么焦虑"},
    "aware of": {"meaning": "意识到", "examples": ["Are you aware of the danger? 你意识到危险了吗？", "I wasn't aware of the problem. 我没意识到这个问题。"], "mnemonic": "意识到(of)什么"},
    "bad at": {"meaning": "不擅长", "examples": ["I'm bad at math. 我不擅长数学。", "He's bad at remembering names. 他不擅长记名字。"], "mnemonic": "在(at)哪方面差"},
    "bored with": {"meaning": "对...厌倦", "examples": ["I'm bored with this game. 我对这个游戏厌倦了。", "The children were bored with the story. 孩子们听烦了这个故事。"], "mnemonic": "跟(with)什么一起无聊"},
    "capable of": {"meaning": "有能力做", "examples": ["She is capable of doing great things. 她有能力做大事。", "Are you capable of handling this? 你能处理这个吗？"], "mnemonic": "能(of)做什么"},
    "confident of": {"meaning": "对...有信心", "examples": ["He's confident of winning. 他对获胜有信心。", "I'm confident of my decision. 我对自己的决定有信心。"], "mnemonic": "对(of)什么有信心"},
    "different from": {"meaning": "与...不同", "examples": ["My opinion is different from yours. 我的观点和你的不同。", "This city is different from what I expected. 这个城市和我想象的不同。"], "mnemonic": "与(from)什么不同"},
    "excited about": {"meaning": "对...兴奋", "examples": ["The kids are excited about the trip. 孩子们对旅行很兴奋。", "I'm excited about starting my new job. 我对开始新工作很兴奋。"], "mnemonic": "关于(about)什么兴奋"},
    "famous for": {"meaning": "因...而著名", "examples": ["Paris is famous for the Eiffel Tower. 巴黎因埃菲尔铁塔而闻名。", "She's famous for her paintings. 她因画作而著名。"], "mnemonic": "因(for)什么而著名"},
    "full of": {"meaning": "充满", "examples": ["The room is full of flowers. 房间满是花。", "He's full of energy. 他充满活力。"], "mnemonic": "满(of)的是什么"},
    "good at": {"meaning": "擅长", "examples": ["She's good at singing. 她擅长唱歌。", "Are you good at sports? 你擅长运动吗？"], "mnemonic": "在(at)哪方面好"},
    "interested in": {"meaning": "对...感兴趣", "examples": ["I'm interested in history. 我对历史感兴趣。", "Are you interested in joining us? 你有兴趣加入我们吗？"], "mnemonic": "在里面(in)感兴趣"},
    "proud of": {"meaning": "为...感到骄傲", "examples": ["I'm proud of you. 我为你骄傲。", "She is proud of her achievements. 她为自己的成就感到骄傲。"], "mnemonic": "为(of)什么骄傲"},
    "responsible for": {"meaning": "对...负责", "examples": ["Who is responsible for this mess? 谁对这混乱负责？", "Parents are responsible for their children. 父母对子女负责。"], "mnemonic": "为(for)什么负责"},
    "similar to": {"meaning": "与...相似", "examples": ["Your idea is similar to mine. 你的想法和我的相似。", "This book is similar to the one I read. 这本书和我读过的相似。"], "mnemonic": "跟(to)什么相似"},
    "tired of": {"meaning": "厌倦", "examples": ["I'm tired of waiting. 我等烦了。", "She's tired of doing the same thing every day. 她厌倦每天做同样的事。"], "mnemonic": "厌(of)了什么"},
    "used to": {"meaning": "习惯于", "examples": ["I'm used to getting up early. 我习惯早起。", "He's not used to the cold weather. 他不习惯寒冷天气。"], "mnemonic": "用过(to)就习惯"},
    "worried about": {"meaning": "担心", "examples": ["Don't be worried about the test. 别担心考试。", "She's worried about her health. 她担心自己的健康。"], "mnemonic": "关于(about)什么担心"},
}

# ---------- 动词+介词搭配素材 ----------
V_PREP = {
    "depend on": {"meaning": "依赖；取决于", "examples": ["Children depend on their parents. 孩子依赖父母。", "It depends on the weather. 这取决于天气。"], "mnemonic": "靠在(on)上面就依赖"},
    "believe in": {"meaning": "相信；信仰", "examples": ["Do you believe in ghosts? 你相信鬼吗？", "I believe in you. 我相信你。"], "mnemonic": "在里面(in)相信"},
    "apply for": {"meaning": "申请", "examples": ["I applied for a job. 我申请了一份工作。", "How do I apply for a visa? 怎么申请签证？"], "mnemonic": "为了(for)什么申请"},
    "care about": {"meaning": "关心；在意", "examples": ["Do you care about the environment? 你关心环境吗？", "I don't care about what others think. 我不在乎别人怎么想。"], "mnemonic": "关于(about)什么在意"},
    "agree with": {"meaning": "同意（某人）", "examples": ["I agree with you. 我同意你。", "Do you agree with the decision? 你同意这个决定吗？"], "mnemonic": "跟(with)谁同意"},
    "agree on": {"meaning": "就...达成一致", "examples": ["We agreed on a date. 我们就日期达成了一致。", "They can't agree on anything. 他们什么都达不成一致。"], "mnemonic": "在(on)什么上同意"},
    "apologize for": {"meaning": "为...道歉", "examples": ["I apologize for being late. 我为迟到道歉。", "He apologized for his mistake. 他为错误道歉。"], "mnemonic": "为(for)什么道歉"},
    "argue about": {"meaning": "争论", "examples": ["They argued about money. 他们为钱争吵。", "There's no point arguing about it. 争论这个没意义。"], "mnemonic": "关于(about)什么争论"},
    "arrive at": {"meaning": "到达（小地方）", "examples": ["We arrived at the station at noon. 我们中午到达车站。", "When did you arrive at the hotel? 你什么时候到酒店的？"], "mnemonic": "到达(at)小地方"},
    "arrive in": {"meaning": "到达（大地方）", "examples": ["They arrived in Beijing yesterday. 他们昨天到了北京。", "When will you arrive in London? 你什么时候到伦敦？"], "mnemonic": "到达(in)大城市"},
    "ask for": {"meaning": "请求；要求", "examples": ["He asked for help. 他请求帮助。", "She asked for a glass of water. 她要了一杯水。"], "mnemonic": "为了(for)什么请求"},
    "belong to": {"meaning": "属于", "examples": ["This book belongs to me. 这本书是我的。", "Who does this bag belong to? 这个包是谁的？"], "mnemonic": "去(to)哪里就属于"},
    "complain about": {"meaning": "抱怨", "examples": ["He always complains about the weather. 他总抱怨天气。", "Stop complaining about everything. 别什么都抱怨。"], "mnemonic": "关于(about)什么抱怨"},
    "consist of": {"meaning": "由...组成", "examples": ["The team consists of five members. 团队由五名成员组成。", "Water consists of hydrogen and oxygen. 水由氢和氧组成。"], "mnemonic": "由(of)什么组成"},
    "deal with": {"meaning": "处理；对付", "examples": ["How do you deal with stress? 你怎么处理压力？", "I'll deal with this problem. 我来处理这个问题。"], "mnemonic": "跟(with)什么打交道"},
    "dream of": {"meaning": "梦想", "examples": ["I dream of becoming a doctor. 我梦想成为医生。", "She dreams of traveling the world. 她梦想环游世界。"], "mnemonic": "关于(of)什么做梦"},
    "insist on": {"meaning": "坚持；执意", "examples": ["She insisted on paying. 她执意要付钱。", "He insists on doing it himself. 他坚持自己做。"], "mnemonic": "在(on)上面坚持"},
    "laugh at": {"meaning": "嘲笑", "examples": ["Don't laugh at others. 不要嘲笑别人。", "Everyone laughed at his joke. 大家都被他的笑话逗笑了。"], "mnemonic": "对着(at)什么笑"},
    "listen to": {"meaning": "听", "examples": ["Listen to me carefully. 仔细听我说。", "I like to listen to music. 我喜欢听音乐。"], "mnemonic": "朝向(to)什么听"},
    "pay for": {"meaning": "为...付钱", "examples": ["Who will pay for the dinner? 谁来付晚餐钱？", "You have to pay for what you broke. 你得为你打破的东西付钱。"], "mnemonic": "为(for)什么付钱"},
    "rely on": {"meaning": "依靠", "examples": ["You can rely on me. 你可以依靠我。", "We rely on technology too much. 我们太依赖科技了。"], "mnemonic": "靠在(on)上面就依赖"},
    "result in": {"meaning": "导致；结果是", "examples": ["The accident resulted in three deaths. 事故导致三人死亡。", "Hard work results in success. 努力会带来成功。"], "mnemonic": "结果在(in)里面就导致"},
    "result from": {"meaning": "由...引起", "examples": ["His illness resulted from stress. 他的病是压力引起的。", "Success results from hard work. 成功源于努力。"], "mnemonic": "结果从(from)哪里来"},
    "suffer from": {"meaning": "遭受；患...病", "examples": ["He suffers from headaches. 他患头痛。", "Many people suffer from poverty. 很多人遭受贫困。"], "mnemonic": "从(from)什么中受苦"},
    "think about": {"meaning": "考虑；思考", "examples": ["Think about it carefully. 仔细想想。", "What are you thinking about? 你在想什么？"], "mnemonic": "关于(about)什么思考"},
    "think of": {"meaning": "想到；想起", "examples": ["I can't think of his name. 我想不起他的名字。", "Think of a number. 想一个数字。"], "mnemonic": "想到(of)什么"},
    "wait for": {"meaning": "等待", "examples": ["I'm waiting for the bus. 我在等公交。", "We waited for hours. 我们等了好几个小时。"], "mnemonic": "为了(for)什么等"},
}

# ---------- 固定短语素材 ----------
FIXED_PHRASES = {
    "in fact": {"meaning": "事实上", "examples": ["In fact, he never went there. 事实上，他根本没去过。", "The exam was, in fact, quite easy. 考试其实很简单。"], "mnemonic": "在(in)事实(fact)中"},
    "as a result": {"meaning": "结果；因此", "examples": ["He worked hard; as a result, he passed. 他努力学习，结果通过了。", "As a result of the rain, the game was canceled. 由于下雨，比赛取消了。"], "mnemonic": "作为(as)一个结果(result)"},
    "in addition": {"meaning": "另外；此外", "examples": ["In addition, you need to bring your ID. 另外，你需要带身份证。", "In addition to English, she speaks French. 除了英语，她还会说法语。"], "mnemonic": "在(in)增加(addition)中"},
    "on the other hand": {"meaning": "另一方面", "examples": ["On the other hand, it could be dangerous. 另一方面，这可能很危险。", "He is smart; on the other hand, he is lazy. 他很聪明；另一方面，他很懒。"], "mnemonic": "在另一只(other hand)手上"},
    "in conclusion": {"meaning": "总之；最后", "examples": ["In conclusion, we should take action now. 总之，我们应该立即行动。", "In conclusion, the plan was a success. 总而言之，计划成功了。"], "mnemonic": "在(in)结论(conclusion)中"},
    "for example": {"meaning": "例如", "examples": ["Take China, for example. 以中国为例。", "For example, you could try learning online. 例如，你可以试试在线学习。"], "mnemonic": "为了(for)举例(example)"},
    "in other words": {"meaning": "换句话说", "examples": ["In other words, he failed the test. 换句话说，他没通过考试。", "In other words, you don't agree. 换句话说，你不同意。"], "mnemonic": "用别的(other)话(words)说"},
    "at the same time": {"meaning": "同时；然而", "examples": ["We arrived at the same time. 我们同时到达。", "I like him, but at the same time I find him annoying. 我喜欢他，但同时觉得他烦人。"], "mnemonic": "在同一个(same)时间(time)"},
    "by the way": {"meaning": "顺便说一句", "examples": ["By the way, have you seen my phone? 顺便问一下，你看到我手机了吗？", "By the way, I met your sister yesterday. 顺便说一下，我昨天遇到你姐姐了。"], "mnemonic": "沿着(by)路(way)顺便说"},
    "in my opinion": {"meaning": "在我看来", "examples": ["In my opinion, this is the best choice. 在我看来，这是最好的选择。", "In my opinion, you should apologize. 依我看，你应该道歉。"], "mnemonic": "在我的(my)观点(opinion)中"},
    "as soon as": {"meaning": "一...就...", "examples": ["Call me as soon as you arrive. 你一到就给我打电话。", "As soon as the bell rang, they ran out. 铃一响他们就跑了出去。"], "mnemonic": "一样(soon)快就发生"},
    "no longer": {"meaning": "不再", "examples": ["He no longer works here. 他不再在这里工作了。", "This phone is no longer supported. 这部手机不再支持了。"], "mnemonic": "没有(no)更长(longer)了"},
    "at least": {"meaning": "至少", "examples": ["You need at least 8 hours of sleep. 你至少需要8小时睡眠。", "At least try before you give up. 至少试试再放弃。"], "mnemonic": "在(at)最少(least)处"},
    "in case": {"meaning": "以防；万一", "examples": ["Take an umbrella in case it rains. 带把伞，万一下雨。", "In case of emergency, call 110. 紧急情况下打110。"], "mnemonic": "在(in)情况(case)中"},
    "so that": {"meaning": "以便；为了", "examples": ["Speak louder so that everyone can hear. 大声点说，让大家都能听到。", "I set the alarm so that I wouldn't oversleep. 我设闹钟是为了不睡过头。"], "mnemonic": "这样(so)就能(that)"},
    "even though": {"meaning": "即使；虽然", "examples": ["Even though it rained, we had fun. 即使下雨了，我们也很开心。", "She went out even though she was tired. 虽然累了，她还是出去了。"], "mnemonic": "甚至(even)虽然(though)"},
    "as if": {"meaning": "好像；仿佛", "examples": ["He talks as if he knows everything. 他说得好像什么都知道。", "It looks as if it's going to rain. 看起来好像要下雨了。"], "mnemonic": "就像(as)如果(if)"},
    "now that": {"meaning": "既然", "examples": ["Now that you're here, let's start. 既然你来了，我们开始吧。", "Now that I think about it, you're right. 现在想想，你是对的。"], "mnemonic": "现在(now)那个(that)既然"},
    "provided that": {"meaning": "如果；假如", "examples": ["You can go, provided that you finish your homework. 只要你做完作业就可以去。", "I'll come, provided that I have time. 如果有时间我就来。"], "mnemonic": "提供(provided)条件(that)"},
}

# ---------- 词语搭配素材 ----------
COLLOCATION_DATA = {
    # make + 名词
    "make a decision": {"meaning": "做决定", "examples": ["You need to make a decision soon. 你需要尽快做决定。", "Making a decision is never easy. 做决定从来不容易。"], "mnemonic": "make(做)决定(decision)"},
    "make a mistake": {"meaning": "犯错误", "examples": ["Everyone makes mistakes. 每个人都会犯错。", "Don't be afraid to make a mistake. 别怕犯错。"], "mnemonic": "make(做)错误(mistake)"},
    "make progress": {"meaning": "取得进步", "examples": ["You're making great progress! 你进步很大！", "We need to make progress on this project. 我们需要推进这个项目。"], "mnemonic": "make(做)进步(progress)"},
    "make an effort": {"meaning": "努力", "examples": ["Please make an effort to be on time. 请努力准时。", "She made a real effort to help. 她真的很努力帮忙。"], "mnemonic": "make(做)努力(effort)"},
    "make a difference": {"meaning": "有影响；起作用", "examples": ["One person can make a difference. 一个人也能改变世界。", "Your donation will make a real difference. 你的捐款会很有帮助。"], "mnemonic": "make(做)不同(difference)"},
    "make sense": {"meaning": "有意义；讲得通", "examples": ["That makes sense. 有道理。", "This sentence doesn't make sense. 这个句子不通。"], "mnemonic": "make(做)感觉(sense)"},
    "make money": {"meaning": "赚钱", "examples": ["How do you make money? 你怎么赚钱的？", "It's not easy to make money. 赚钱不容易。"], "mnemonic": "make(做)钱(money)"},
    "make friends": {"meaning": "交朋友", "examples": ["It's easy to make friends here. 在这里容易交朋友。", "She made a lot of new friends at school. 她在学校交了很多新朋友。"], "mnemonic": "make(做)朋友(friends)"},
    "make a suggestion": {"meaning": "提建议", "examples": ["Can I make a suggestion? 我能提个建议吗？", "She made a helpful suggestion. 她提了个有用的建议。"], "mnemonic": "make(做)建议(suggestion)"},
    "make an appointment": {"meaning": "预约", "examples": ["I'd like to make an appointment. 我想预约。", "Did you make an appointment with the doctor? 你跟医生预约了吗？"], "mnemonic": "make(做)预约(appointment)"},
    "make a speech": {"meaning": "发表演讲", "examples": ["He made a speech at the wedding. 他在婚礼上发表了演讲。", "I'm nervous about making a speech. 我对演讲感到紧张。"], "mnemonic": "make(做)演讲(speech)"},
    "make a plan": {"meaning": "制定计划", "examples": ["Let's make a plan for the weekend. 我们来制定周末计划。", "You should make a plan before you start. 开始前应该做个计划。"], "mnemonic": "make(做)计划(plan)"},
    "make a promise": {"meaning": "许诺", "examples": ["Don't make a promise you can't keep. 别许你做不到的诺。", "He made a promise to his mother. 他对母亲许了诺。"], "mnemonic": "make(做)承诺(promise)"},
    "make a complaint": {"meaning": "投诉；抱怨", "examples": ["I want to make a complaint about the service. 我想投诉服务。", "She made a complaint to the manager. 她向经理投诉了。"], "mnemonic": "make(做)投诉(complaint)"},
    "make an excuse": {"meaning": "找借口", "examples": ["He always makes excuses for being late. 他总为迟到找借口。", "Stop making excuses! 别找借口了！"], "mnemonic": "make(做)借口(excuse)"},

    # do + 名词
    "do homework": {"meaning": "做作业", "examples": ["I need to do my homework. 我要做作业。", "Have you done your homework? 你作业做完了吗？"], "mnemonic": "do(做)作业(homework)"},
    "do business": {"meaning": "做生意", "examples": ["We do business with many countries. 我们和很多国家做生意。", "It's a pleasure doing business with you. 很荣幸和你做生意。"], "mnemonic": "do(做)生意(business)"},
    "do harm": {"meaning": "造成伤害", "examples": ["Smoking does harm to your health. 吸烟有害健康。", "This policy will do more harm than good. 这政策弊大于利。"], "mnemonic": "do(做)伤害(harm)"},
    "do good": {"meaning": "做好事；有益", "examples": ["Exercise does good to your body. 运动对身体好。", "She likes to do good in the community. 她喜欢在社区做好事。"], "mnemonic": "do(做)好事(good)"},
    "do a favor": {"meaning": "帮忙", "examples": ["Can you do me a favor? 你能帮我个忙吗？", "Thanks for doing me a favor. 谢谢你帮忙。"], "mnemonic": "do(做)帮忙(favor)"},
    "do the dishes": {"meaning": "洗碗", "examples": ["I'll do the dishes tonight. 今晚我来洗碗。", "Who's going to do the dishes? 谁来洗碗？"], "mnemonic": "do(做)盘子(dishes)"},
    "do the laundry": {"meaning": "洗衣服", "examples": ["I need to do the laundry. 我要洗衣服。", "She does the laundry every Sunday. 她每周日洗衣服。"], "mnemonic": "do(做)洗衣(laundry)"},
    "do research": {"meaning": "做研究", "examples": ["She's doing research on cancer. 她在做癌症研究。", "I need to do more research. 我需要做更多研究。"], "mnemonic": "do(做)研究(research)"},
    "do an experiment": {"meaning": "做实验", "examples": ["We did an experiment in science class. 我们在科学课上做了实验。", "The scientists did several experiments. 科学家做了几个实验。"], "mnemonic": "do(做)实验(experiment)"},
    "do well": {"meaning": "做得好；表现好", "examples": ["You did well on the test! 你考试考得很好！", "She's doing well in her new job. 她在新工作中表现很好。"], "mnemonic": "do(做)好(well)"},
    "do one's best": {"meaning": "尽力", "examples": ["Just do your best! 尽力就好！", "I did my best, but I still failed. 我尽力了，但还是没成功。"], "mnemonic": "do(做)最好(best)"},

    # have + 名词
    "have a break": {"meaning": "休息一下", "examples": ["Let's have a break. 我们休息一下吧。", "You should have a break every hour. 每小时应该休息一下。"], "mnemonic": "have(有)休息(break)"},
    "have a look": {"meaning": "看一看", "examples": ["Can I have a look? 我能看看吗？", "Let's have a look at the map. 我们看看地图。"], "mnemonic": "have(有)一看(look)"},
    "have a rest": {"meaning": "休息", "examples": ["You look tired. Have a rest. 你看起来累了，休息吧。", "She had a short rest after lunch. 午饭后她休息了一会儿。"], "mnemonic": "have(有)休息(rest)"},
    "have a try": {"meaning": "试一试", "examples": ["Let me have a try. 让我试试。", "Why don't you have a try? 你为什么不试试？"], "mnemonic": "have(有)一试(try)"},
    "have a chat": {"meaning": "聊天", "examples": ["Let's have a chat after class. 下课后我们聊聊天。", "We had a nice chat over coffee. 我们喝咖啡聊得很愉快。"], "mnemonic": "have(有)聊天(chat)"},
    "have a bath": {"meaning": "洗澡", "examples": ["I usually have a bath before bed. 我通常睡前洗澡。", "She had a long hot bath. 她洗了个长时间的热水澡。"], "mnemonic": "have(有)洗澡(bath)"},
    "have a party": {"meaning": "开派对", "examples": ["We're having a party this Saturday. 这周六我们开派对。", "They had a birthday party for her. 他们为她办了生日派对。"], "mnemonic": "have(有)派对(party)"},
    "have fun": {"meaning": "玩得开心", "examples": ["Have fun at the park! 在公园玩开心！", "We had a lot of fun yesterday. 昨天我们玩得很开心。"], "mnemonic": "have(有)乐趣(fun)"},
    "have trouble": {"meaning": "有困难；遇到麻烦", "examples": ["I'm having trouble with this math problem. 我这道数学题有困难。", "She had trouble finding the place. 她找不到地方。"], "mnemonic": "have(有)麻烦(trouble)"},
    "have difficulty": {"meaning": "有困难", "examples": ["He has difficulty reading small print. 他看小字有困难。", "They had difficulty communicating. 他们沟通有困难。"], "mnemonic": "have(有)困难(difficulty)"},
    "have an effect": {"meaning": "有影响", "examples": ["The medicine had an immediate effect. 药立刻见效了。", "His words had a deep effect on me. 他的话对我影响很深。"], "mnemonic": "have(有)效果(effect)"},
    "have an impact": {"meaning": "有影响", "examples": ["Technology has a huge impact on our lives. 科技对我们的生活影响巨大。", "The speech had a strong impact on the audience. 演讲对观众影响很大。"], "mnemonic": "have(有)影响(impact)"},
    "have access": {"meaning": "有权使用；可以进入", "examples": ["Students have access to the library. 学生可以使用图书馆。", "Do you have access to the internet? 你能上网吗？"], "mnemonic": "have(有)通道(access)"},
    "have the ability": {"meaning": "有能力", "examples": ["She has the ability to lead. 她有能力领导。", "Do you have the ability to solve this? 你有能力解决这个吗？"], "mnemonic": "have(有)能力(ability)"},
    "have a chance": {"meaning": "有机会", "examples": ["I never had a chance to thank him. 我从没机会感谢他。", "Everyone should have a chance to succeed. 每个人都应该有成功的机会。"], "mnemonic": "have(有)机会(chance)"},
    "have a problem": {"meaning": "有问题", "examples": ["Do you have a problem with that? 你对此有意见吗？", "I have a problem with my computer. 我的电脑有问题。"], "mnemonic": "have(有)问题(problem)"},

    # take + 名词
    "take a shower": {"meaning": "洗澡（淋浴）", "examples": ["I usually take a shower in the morning. 我通常早上洗澡。", "He took a quick shower before going out. 他出门前快速洗了个澡。"], "mnemonic": "take(拿)淋浴(shower)"},
    "take a walk": {"meaning": "散步", "examples": ["Let's take a walk after dinner. 晚饭后我们散散步。", "She takes a walk in the park every day. 她每天在公园散步。"], "mnemonic": "take(拿)散步(walk)"},
    "take a nap": {"meaning": "小睡；午睡", "examples": ["I need to take a nap. 我需要小睡一会儿。", "The baby takes a nap every afternoon. 宝宝每天下午小睡。"], "mnemonic": "take(拿)小睡(nap)"},
    "take a break": {"meaning": "休息", "examples": ["Let's take a break. 我们休息一下。", "You should take a break from studying. 你应该休息一下别学了。"], "mnemonic": "take(拿)休息(break)"},
    "take a risk": {"meaning": "冒险", "examples": ["Sometimes you have to take a risk. 有时候你得冒险。", "He took a big risk starting his own business. 他创业冒了很大风险。"], "mnemonic": "take(拿)风险(risk)"},
    "take a photo": {"meaning": "拍照", "examples": ["Can you take a photo of us? 你能给我们拍张照吗？", "She took a lot of photos on her trip. 她旅行拍了很多照片。"], "mnemonic": "take(拿)照片(photo)"},
    "take a test": {"meaning": "参加考试", "examples": ["We're taking a test tomorrow. 我们明天考试。", "How did you do on the test you took? 你考试考得怎样？"], "mnemonic": "take(拿)考试(test)"},
    "take medicine": {"meaning": "吃药", "examples": ["Remember to take your medicine. 记得吃药。", "She takes medicine three times a day. 她一天吃三次药。"], "mnemonic": "take(拿)药(medicine)"},
    "take responsibility": {"meaning": "承担责任", "examples": ["You need to take responsibility for your actions. 你需要为自己的行为负责。", "He took full responsibility for the mistake. 他对错误承担了全部责任。"], "mnemonic": "take(拿)责任(responsibility)"},
    "take action": {"meaning": "采取行动", "examples": ["It's time to take action. 该采取行动了。", "The government took action to reduce pollution. 政府采取行动减少污染。"], "mnemonic": "take(拿)行动(action)"},
    "take notes": {"meaning": "记笔记", "examples": ["Don't forget to take notes in class. 上课别忘了记笔记。", "She took careful notes during the meeting. 会议中她认真记了笔记。"], "mnemonic": "take(拿)笔记(notes)"},
    "take a deep breath": {"meaning": "深呼吸", "examples": ["Take a deep breath and relax. 深呼吸，放松。", "She took a deep breath before going on stage. 上台前她深吸了一口气。"], "mnemonic": "take(拿)深呼吸(deep breath)"},
    "take pride in": {"meaning": "以...为傲", "examples": ["She takes pride in her work. 她以工作为傲。", "We take pride in our school. 我们以学校为傲。"], "mnemonic": "take(拿)骄傲(pride)在(in)里面"},
    "take part in": {"meaning": "参加", "examples": ["Will you take part in the contest? 你会参加比赛吗？", "Many students took part in the event. 很多学生参加了活动。"], "mnemonic": "take(拿)部分(part)在(in)里面"},
    "take advantage of": {"meaning": "利用", "examples": ["Take advantage of this opportunity. 利用这个机会。", "He took advantage of her kindness. 他利用了她的善良。"], "mnemonic": "take(拿)优势(advantage)的(of)"},
    "take into account": {"meaning": "考虑到", "examples": ["We must take all factors into account. 我们必须考虑所有因素。", "His age was taken into account. 他的年龄被考虑在内。"], "mnemonic": "take(拿)进入(into)账户(account)"},
    "take care of": {"meaning": "照顾；处理", "examples": ["Who will take care of the baby? 谁来照顾宝宝？", "I'll take care of the paperwork. 我来处理文件。"], "mnemonic": "take(拿)关心(care)的(of)"},

    # 其他高频搭配
    "pay attention": {"meaning": "注意", "examples": ["Pay attention in class! 上课注意听讲！", "You need to pay attention to details. 你需要注意细节。"], "mnemonic": "pay(付)注意(attention)"},
    "pay a visit": {"meaning": "拜访", "examples": ["Let's pay a visit to your grandma. 我们去看看奶奶吧。", "We paid a visit to the museum. 我们参观了博物馆。"], "mnemonic": "pay(付)访问(visit)"},
    "pay the price": {"meaning": "付出代价", "examples": ["If you don't study, you'll pay the price. 不学习就会付出代价。", "He paid the price for his laziness. 他为懒惰付出了代价。"], "mnemonic": "pay(付)代价(price)"},
    "keep a secret": {"meaning": "保守秘密", "examples": ["Can you keep a secret? 你能保密吗？", "She promised to keep the secret. 她答应保守秘密。"], "mnemonic": "keep(保持)秘密(secret)"},
    "keep a promise": {"meaning": "遵守承诺", "examples": ["You should always keep your promise. 你应该始终遵守承诺。", "He never keeps his promises. 他从不遵守承诺。"], "mnemonic": "keep(保持)承诺(promise)"},
    "keep a diary": {"meaning": "写日记", "examples": ["I keep a diary every day. 我每天写日记。", "She's kept a diary since she was 10. 她从10岁起就写日记。"], "mnemonic": "keep(保持)日记(diary)"},
    "keep in touch": {"meaning": "保持联系", "examples": ["Let's keep in touch! 保持联系！", "We've kept in touch for 20 years. 我们保持联系20年了。"], "mnemonic": "keep(保持)在(in)接触(touch)"},
    "keep an eye on": {"meaning": "留意；照看", "examples": ["Keep an eye on the kids. 看好孩子。", "Can you keep an eye on my bag? 能帮我看一下包吗？"], "mnemonic": "keep(保持)一只眼(eye)在(on)上面"},
    "catch a cold": {"meaning": "感冒", "examples": ["I think I caught a cold. 我想我感冒了。", "Wear a coat or you'll catch a cold. 穿上外套，不然会感冒。"], "mnemonic": "catch(抓)感冒(cold)"},
    "catch fire": {"meaning": "着火", "examples": ["The building caught fire last night. 大楼昨晚着火了。", "Be careful, paper catches fire easily. 小心，纸容易着火。"], "mnemonic": "catch(抓)火(fire)"},
    "catch the bus": {"meaning": "赶公交", "examples": ["Hurry up! We need to catch the bus. 快点！我们要赶公交。", "I missed my stop because I fell asleep on the bus. 我因为在公交上睡着了而错过了站。"], "mnemonic": "catch(抓)公交车(bus)"},
    "break a record": {"meaning": "打破纪录", "examples": ["He broke the world record. 他打破了世界纪录。", "The movie broke box office records. 电影打破了票房纪录。"], "mnemonic": "break(打破)纪录(record)"},
    "break the law": {"meaning": "违法", "examples": ["If you break the law, you'll be punished. 违法就会受罚。", "He didn't know he was breaking the law. 他不知道自己在违法。"], "mnemonic": "break(打破)法律(law)"},
    "break a promise": {"meaning": "违背承诺", "examples": ["Don't break your promise. 不要违背承诺。", "He broke his promise to help. 他违背了帮忙的承诺。"], "mnemonic": "break(打破)承诺(promise)"},
    "break the silence": {"meaning": "打破沉默", "examples": ["A loud noise broke the silence. 一声巨响打破了沉默。", "She broke the silence with a joke. 她用笑话打破了沉默。"], "mnemonic": "break(打破)沉默(silence)"},
    "break a habit": {"meaning": "改掉习惯", "examples": ["It's hard to break a bad habit. 改掉坏习惯很难。", "She finally broke the habit of biting her nails. 她终于改掉了咬指甲的习惯。"], "mnemonic": "break(打破)习惯(habit)"},
    "break the ice": {"meaning": "打破僵局；活跃气氛", "examples": ["He told a joke to break the ice. 他讲了个笑话活跃气氛。", "A good icebreaker can break the ice. 好的开场白能打破僵局。"], "mnemonic": "break(打破)冰(ice)"},
    "set a record": {"meaning": "创纪录", "examples": ["She set a new school record. 她创造了新的校纪录。", "The company set a record for sales. 公司创下了销售纪录。"], "mnemonic": "set(设置)纪录(record)"},
    "set an example": {"meaning": "树立榜样", "examples": ["Parents should set a good example. 父母应该树立好榜样。", "She set an example for other students. 她为其他学生树立了榜样。"], "mnemonic": "set(设置)榜样(example)"},
    "set a goal": {"meaning": "设定目标", "examples": ["You should set a goal for yourself. 你应该给自己设定目标。", "They set a goal of finishing in a month. 他们设定了一个月完成的目标。"], "mnemonic": "set(设置)目标(goal)"},
    "set a standard": {"meaning": "制定标准", "examples": ["The school sets high standards. 学校制定了高标准。", "We need to set clear standards. 我们需要制定明确的标准。"], "mnemonic": "set(设置)标准(standard)"},
    "set a trend": {"meaning": "引领潮流", "examples": ["This designer always sets trends. 这位设计师总是引领潮流。", "Social media set a new trend. 社交媒体引领了新潮流。"], "mnemonic": "set(设置)潮流(trend)"},
    "draw a conclusion": {"meaning": "得出结论", "examples": ["What conclusion can you draw? 你能得出什么结论？", "Scientists drew a different conclusion. 科学家得出了不同的结论。"], "mnemonic": "draw(画)结论(conclusion)"},
    "draw attention": {"meaning": "吸引注意", "examples": ["The sign drew my attention. 标志吸引了我的注意。", "She likes to draw attention to herself. 她喜欢引人注目。"], "mnemonic": "draw(拉)注意力(attention)"},
    "draw a line": {"meaning": "画线；划清界限", "examples": ["Draw a line under your answer. 在你的答案下画一条线。", "You have to draw a line somewhere. 你总得有个底线。"], "mnemonic": "draw(画)线(line)"},
    "draw a picture": {"meaning": "画画", "examples": ["Can you draw a picture of a cat? 你能画一只猫吗？", "The child drew a picture of his family. 孩子画了一幅全家福。"], "mnemonic": "draw(画)图(picture)"},
    "raise a question": {"meaning": "提出问题", "examples": ["I'd like to raise a question. 我想提一个问题。", "The report raises several important questions. 报告提出了几个重要问题。"], "mnemonic": "raise(举起)问题(question)"},
    "raise awareness": {"meaning": "提高意识", "examples": ["The campaign raises awareness about pollution. 这个活动提高人们对污染的意识。", "We need to raise awareness of this issue. 我们需要提高对这个问题的意识。"], "mnemonic": "raise(提高)意识(awareness)"},
    "raise money": {"meaning": "筹款", "examples": ["They're raising money for charity. 他们在为慈善筹款。", "The event raised over $10,000. 活动筹到了超过一万美元。"], "mnemonic": "raise(举起)钱(money)"},
    "raise a family": {"meaning": "养家", "examples": ["It's hard to raise a family these days. 如今养家很难。", "She raised a family of five. 她养了五口之家。"], "mnemonic": "raise(举起)家庭(family)"},
    "raise your voice": {"meaning": "提高嗓门", "examples": ["Don't raise your voice at me. 别对我大声嚷嚷。", "He rarely raises his voice. 他很少大声说话。"], "mnemonic": "raise(提高)声音(voice)"},
    "reach a decision": {"meaning": "做出决定", "examples": ["We need to reach a decision soon. 我们需要尽快做出决定。", "After hours of discussion, they reached a decision. 讨论数小时后他们做出了决定。"], "mnemonic": "reach(到达)决定(decision)"},
    "reach an agreement": {"meaning": "达成协议", "examples": ["The two sides reached an agreement. 双方达成了协议。", "We finally reached an agreement on the price. 我们终于在价格上达成了一致。"], "mnemonic": "reach(到达)协议(agreement)"},
    "reach a compromise": {"meaning": "达成妥协", "examples": ["We need to reach a compromise. 我们需要达成妥协。", "They reached a compromise after long talks. 长谈后他们达成了妥协。"], "mnemonic": "reach(到达)妥协(compromise)"},
    "reach a goal": {"meaning": "实现目标", "examples": ["She finally reached her goal. 她终于实现了目标。", "Hard work helps you reach your goals. 努力有助于实现目标。"], "mnemonic": "reach(到达)目标(goal)"},
    "meet a deadline": {"meaning": "按时完成；赶上截止日期", "examples": ["We must meet the deadline. 我们必须按时完成。", "She always meets her deadlines. 她总是能按时完成。"], "mnemonic": "meet(遇见)截止日期(deadline)"},
    "meet requirements": {"meaning": "满足要求", "examples": ["Does this product meet the requirements? 这个产品符合要求吗？", "You must meet all the requirements. 你必须满足所有要求。"], "mnemonic": "meet(遇见)要求(requirements)"},
    "meet expectations": {"meaning": "达到期望", "examples": ["The movie didn't meet my expectations. 电影没有达到我的期望。", "Our results met all expectations. 我们的结果达到了所有期望。"], "mnemonic": "meet(遇见)期望(expectations)"},
    "meet standards": {"meaning": "达到标准", "examples": ["The product meets international standards. 产品达到国际标准。", "Your work doesn't meet our standards. 你的工作没达到我们的标准。"], "mnemonic": "meet(遇见)标准(standards)"},
    "meet needs": {"meaning": "满足需求", "examples": ["We try to meet the needs of all students. 我们尽量满足所有学生的需求。", "This design meets our needs perfectly. 这个设计完美满足我们的需求。"], "mnemonic": "meet(遇见)需求(needs)"},
    "meet a challenge": {"meaning": "迎接挑战", "examples": ["Are you ready to meet the challenge? 你准备好迎接挑战了吗？", "She met every challenge with confidence. 她自信地迎接每一个挑战。"], "mnemonic": "meet(遇见)挑战(challenge)"},
    "face a problem": {"meaning": "面对问题", "examples": ["We're facing a serious problem. 我们正面临一个严重问题。", "Don't run away—face your problems. 别逃避——面对你的问题。"], "mnemonic": "face(面对)问题(problem)"},
    "face a challenge": {"meaning": "面对挑战", "examples": ["Everyone faces challenges in life. 每个人生活中都会面临挑战。", "We need to face this challenge together. 我们需要一起面对这个挑战。"], "mnemonic": "face(面对)挑战(challenge)"},
    "face the truth": {"meaning": "面对真相", "examples": ["You have to face the truth. 你必须面对真相。", "It's time to face the truth. 是时候面对现实了。"], "mnemonic": "face(面对)真相(truth)"},
    "face the consequences": {"meaning": "承担后果", "examples": ["If you break the rules, you must face the consequences. 违反规则就必须承担后果。", "He's not ready to face the consequences. 他还没准备好承担后果。"], "mnemonic": "face(面对)后果(consequences)"},
    "solve a problem": {"meaning": "解决问题", "examples": ["Can you solve this math problem? 你能解这道数学题吗？", "We need to solve the problem quickly. 我们需要尽快解决问题。"], "mnemonic": "solve(解决)问题(problem)"},
    "solve a mystery": {"meaning": "解开谜团", "examples": ["The detective solved the mystery. 侦探解开了谜团。", "Scientists are trying to solve the mystery. 科学家正在试图解开谜团。"], "mnemonic": "solve(解决)谜团(mystery)"},
    "address a problem": {"meaning": "处理问题", "examples": ["We need to address this problem immediately. 我们需要立刻处理这个问题。", "The government is addressing the housing problem. 政府正在处理住房问题。"], "mnemonic": "address(处理)问题(problem)"},
    "address an issue": {"meaning": "处理议题", "examples": ["The meeting will address this issue. 会议将处理这个议题。", "We must address the issue of climate change. 我们必须处理气候变化问题。"], "mnemonic": "address(处理)议题(issue)"},
    "address concerns": {"meaning": "处理担忧", "examples": ["The company addressed customers' concerns. 公司处理了客户的担忧。", "We need to address your concerns. 我们需要处理你的担忧。"], "mnemonic": "address(处理)担忧(concerns)"},
    "express an opinion": {"meaning": "表达意见", "examples": ["Everyone has the right to express an opinion. 每个人都有表达意见的权利。", "She expressed her opinion clearly. 她清楚地表达了自己的意见。"], "mnemonic": "express(表达)意见(opinion)"},
    "express concern": {"meaning": "表达关切", "examples": ["Doctors expressed concern about the outbreak. 医生对疫情表达了关切。", "He expressed concern for his friend's health. 他对朋友的健康表达了关心。"], "mnemonic": "express(表达)关切(concern)"},
    "express gratitude": {"meaning": "表达感谢", "examples": ["I'd like to express my gratitude to everyone. 我想感谢每一个人。", "She expressed her gratitude with a smile. 她用微笑表达了感谢。"], "mnemonic": "express(表达)感谢(gratitude)"},
    "express interest": {"meaning": "表达兴趣", "examples": ["Many people expressed interest in the job. 很多人对这份工作表达了兴趣。", "He expressed interest in learning Chinese. 他表达了学中文的兴趣。"], "mnemonic": "express(表达)兴趣(interest)"},
}

# ============================================================
# 题目生成逻辑
# ============================================================

DIFF_LEVELS = ["easy", "medium", "hard", "cet4", "cet6"]
DIFF_WEIGHTS = [0.25, 0.35, 0.20, 0.10, 0.10]
QUESTION_TYPES = ["mc", "fill"]
TYPE_WEIGHTS = [0.65, 0.35]

current_id = 0

def generate_mc_question(phrase, data, cat, diff, explain_template=None):
    """生成选择题"""
    global current_id
    current_id += 1

    # 构造题目
    if cat == "phrase":
        if data.get("meaning"):
            q = f'"{phrase}" 的意思是？'
        else:
            q = f'选择正确的短语填空：He decided to ___. (选择最合适的短语)'
    else:
        q = f'选择与 "{phrase}" 搭配最合适的词：'

    # 生成选项
    correct = f'{phrase} — {data.get("meaning", "")}'
    options = [correct]

    # 生成干扰项
    distractors = generate_distractors(phrase, cat)
    options.extend(distractors[:3])
    random.shuffle(options)

    answer_idx = options.index(correct)

    # 解释
    if explain_template:
        explain = explain_template
    else:
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')

    # 选项标签
    labeled_options = []
    for i, opt in enumerate(options):
        labeled_options.append(f'{opt}')

    return {
        "id": current_id,
        "cat": cat,
        "diff": diff,
        "type": "mc",
        "q": q,
        "hint": data.get("meaning", ""),
        "options": labeled_options,
        "answer": answer_idx,
        "explain": explain,
        "examples": data.get("examples", []),
        "mnemonic": data.get("mnemonic", "")
    }

def generate_fill_question(phrase, data, cat, diff, explain_template=None):
    """生成填空题"""
    global current_id
    current_id += 1

    parts = phrase.split()
    blank_phrase = parts[0] + " _____"

    if len(parts) > 2:
        blank_phrase = parts[0] + " " + " ".join(["_____"] * (len(parts) - 1))
    elif len(parts) == 2:
        blank_phrase = parts[0] + " _____"

    q = f'填入正确的词完成短语：{blank_phrase}（含义：{data.get("meaning", "")}）'

    correct = parts[-1] if len(parts) == 2 else " ".join(parts[1:])

    # 干扰项
    distractors = generate_distractors_for_fill(phrase, cat)
    distractors = [d for d in distractors if d != correct]

    options = [correct] + distractors[:3]
    random.shuffle(options)
    answer_idx = options.index(correct)

    if explain_template:
        explain = explain_template
    else:
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')

    return {
        "id": current_id,
        "cat": cat,
        "diff": diff,
        "type": "fill",
        "q": q,
        "hint": data.get("meaning", ""),
        "options": options,
        "answer": answer_idx,
        "explain": explain,
        "examples": data.get("examples", []),
        "mnemonic": data.get("mnemonic", "")
    }

def generate_distractors(phrase, cat):
    """为选择题生成干扰选项"""
    all_particles = ["up", "down", "off", "on", "out", "in", "away", "over", "through", "by",
                     "into", "back", "for", "after", "before", "with", "about", "of", "to",
                     "from", "at", "along", "forward", "together"]

    all_verbs = ["give", "take", "turn", "put", "get", "look", "bring", "call", "carry",
                 "come", "cut", "drop", "figure", "find", "go", "hold", "keep", "let",
                 "make", "pass", "pick", "point", "pull", "run", "set", "show", "stand",
                 "break", "work", "check", "fill", "hang", "pay", "rule", "settle", "slow",
                 "speed", "think", "throw", "try", "wake", "watch", "wear"]

    distractors = []

    if cat == "phrase":
        words = phrase.split()
        if len(words) == 2:
            verb = words[0]
            particle = words[1]
            # 替换介词/副词
            for p in all_particles:
                if p != particle:
                    distractors.append(f'{verb} {p} — (错误搭配)')
            # 替换动词
            for v in all_verbs:
                if v != verb and random.random() < 0.3:
                    distractors.append(f'{v} {particle} — (错误搭配)')
        elif len(words) == 3:
            # 三词短语
            distractors.append(f'{words[0]} {words[1]} for — (错误搭配)')
            distractors.append(f'{words[0]} {words[2]} to — (错误搭配)')
            distractors.append(f'{words[0]} for {words[2]} — (错误搭配)')
        elif len(words) == 4:
            distractors.append(f'{words[0]} {words[1]} of {words[3]} — (错误搭配)')
            distractors.append(f'{words[0]} {words[2]} for {words[3]} — (错误搭配)')
            distractors.append(f'{words[0]} for {words[2]} {words[3]} — (错误搭配)')
    else:
        # collocation
        distractors.append(f'{phrase.split()[0]} — (错误搭配)')
        distractors.append(f'do {phrase.split()[-1]} — (错误搭配)')
        distractors.append(f'have {phrase.split()[-1]} — (错误搭配)')

    # 去重
    seen = set()
    unique = []
    for d in distractors:
        if d not in seen:
            seen.add(d)
            unique.append(d)

    if len(unique) < 3:
        # 补充通用干扰项
        extra = [
            "look for — (错误搭配)",
            "get up — (错误搭配)",
            "put on — (错误搭配)",
            "take off — (错误搭配)",
            "give away — (错误搭配)",
            "make up — (错误搭配)",
            "turn on — (错误搭配)",
            "break down — (错误搭配)",
        ]
        for e in extra:
            if e not in seen:
                seen.add(e)
                unique.append(e)
                if len(unique) >= 4:
                    break

    return unique[:6]

def generate_distractors_for_fill(phrase, cat):
    """为填空题生成干扰选项"""
    all_particles = ["up", "down", "off", "on", "out", "in", "away", "over", "through", "by",
                     "into", "back", "for", "after", "before", "with", "about", "of", "to",
                     "from", "at", "along", "forward", "together"]
    parts = phrase.split()
    if len(parts) >= 2:
        correct = parts[-1] if len(parts) == 2 else " ".join(parts[1:])
        distractors = [p for p in all_particles if p != correct]
        random.shuffle(distractors)
        return distractors[:5]
    return ["up", "down", "off", "on", "out"]

def pick_diff(index, total):
    """按权重分配难度"""
    r = random.random()
    cumulative = 0
    for i, w in enumerate(DIFF_WEIGHTS):
        cumulative += w
        if r <= cumulative:
            return DIFF_LEVELS[i]
    return DIFF_LEVELS[-1]

def pick_type():
    """按权重分配题型"""
    r = random.random()
    if r <= TYPE_WEIGHTS[0]:
        return "mc"
    return "fill"

# ============================================================
# 主生成逻辑
# ============================================================

def generate_phrases():
    global current_id
    current_id = 0
    questions = []

    # 1. 动词短语
    vp_items = list(VP_DATA.items())
    random.shuffle(vp_items)

    for phrase, data in vp_items[:150]:
        diff = pick_diff(current_id, 500)
        qtype = pick_type()
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "phrase", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "phrase", diff, explain)
        questions.append(q)

    # 2. 形容词+介词搭配
    adj_items = list(ADJ_PREP.items())
    random.shuffle(adj_items)
    for phrase, data in adj_items[:30]:
        diff = pick_diff(current_id, 500)
        qtype = pick_type()
        explain = f'{phrase} = {data.get("meaning", "")}'
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "phrase", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "phrase", diff, explain)
        questions.append(q)

    # 3. 动词+介词
    vprep_items = list(V_PREP.items())
    random.shuffle(vprep_items)
    for phrase, data in vprep_items[:40]:
        diff = pick_diff(current_id, 500)
        qtype = pick_type()
        explain = f'{phrase} = {data.get("meaning", "")}'
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "phrase", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "phrase", diff, explain)
        questions.append(q)

    # 4. 固定短语
    fp_items = list(FIXED_PHRASES.items())
    random.shuffle(fp_items)
    for phrase, data in fp_items[:30]:
        diff = pick_diff(current_id, 500)
        qtype = pick_type()
        explain = f'{phrase} = {data.get("meaning", "")}'
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "phrase", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "phrase", diff, explain)
        questions.append(q)

    # 补充到500题
    while len(questions) < 500:
        # 用变体生成
        source = list(VP_DATA.items()) + list(ADJ_PREP.items()) + list(V_PREP.items()) + list(FIXED_PHRASES.items())
        random.shuffle(source)
        phrase, data = source[0]
        diff = pick_diff(len(questions), 500)
        qtype = pick_type()
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "phrase", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "phrase", diff, explain)
        questions.append(q)

    # 截取前500
    questions = questions[:500]

    # 重新编号
    for i, q in enumerate(questions):
        q["id"] = i + 1

    # 随机打乱
    random.shuffle(questions)

    # 重新编号
    for i, q in enumerate(questions):
        q["id"] = i + 1

    return questions

def generate_collocations():
    global current_id
    current_id = 0
    questions = []

    # 生成所有搭配题目
    coll_items = list(COLLOCATION_DATA.items())
    random.shuffle(coll_items)

    # 先用所有素材生成一轮
    for phrase, data in coll_items:
        diff = pick_diff(current_id, 500)
        qtype = pick_type()
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "collocation", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "collocation", diff, explain)
        questions.append(q)

    # 补充到500
    while len(questions) < 500:
        phrase, data = random.choice(coll_items)
        diff = pick_diff(len(questions), 500)
        qtype = pick_type()
        explain = data.get("explain", f'{phrase} = {data.get("meaning", "")}')
        if qtype == "mc":
            q = generate_mc_question(phrase, data, "collocation", diff, explain)
        else:
            q = generate_fill_question(phrase, data, "collocation", diff, explain)
        questions.append(q)

    questions = questions[:500]

    # 重新编号
    for i, q in enumerate(questions):
        q["id"] = i + 1

    random.shuffle(questions)
    for i, q in enumerate(questions):
        q["id"] = i + 1

    return questions

# ============================================================
# 统计和质量检查
# ============================================================

def print_stats(questions, name):
    print(f"\n{'='*50}")
    print(f"{name}: {len(questions)} 题")
    print(f"{'='*50}")

    # 按难度统计
    diff_counts = {}
    for q in questions:
        d = q["diff"]
        diff_counts[d] = diff_counts.get(d, 0) + 1
    for d in DIFF_LEVELS:
        cnt = diff_counts.get(d, 0)
        print(f"  {d}: {cnt} ({cnt/len(questions)*100:.1f}%)")

    # 按题型统计
    type_counts = {}
    for q in questions:
        t = q["type"]
        type_counts[t] = type_counts.get(t, 0) + 1
    for t in QUESTION_TYPES:
        cnt = type_counts.get(t, 0)
        print(f"  {t}: {cnt} ({cnt/len(questions)*100:.1f}%)")

    # 检查必要字段
    required_fields = ["id", "cat", "diff", "type", "q", "hint", "options", "answer", "explain", "examples", "mnemonic"]
    missing = 0
    for q in questions:
        for f in required_fields:
            if f not in q:
                print(f"  WARNING: 题目 {q.get('id', '?')} 缺少字段 {f}")
                missing += 1

    if missing == 0:
        print(f"  All required fields present.")

    # 验证 answer 范围
    bad_answer = 0
    for q in questions:
        if q["answer"] < 0 or q["answer"] >= len(q["options"]):
            print(f"  WARNING: 题目 {q['id']} answer {q['answer']} 超出 options 范围 {len(q['options'])}")
            bad_answer += 1

    if bad_answer == 0:
        print(f"  All answers valid.")

    # 检查例句
    examples_count = sum(1 for q in questions if q.get("examples") and len(q["examples"]) >= 2)
    print(f"  有2+例句的题目: {examples_count}/{len(questions)}")

    # 检查口诀
    mnemonic_count = sum(1 for q in questions if q.get("mnemonic") and len(q["mnemonic"]) > 0)
    print(f"  有口诀的题目: {mnemonic_count}/{len(questions)}")

# ============================================================
# Main
# ============================================================

if __name__ == "__main__":
    print("Generating phrases.json...")
    phrases = generate_phrases()
    phrases_path = os.path.join(OUTPUT_DIR, "phrases.json")
    with open(phrases_path, "w", encoding="utf-8") as f:
        json.dump(phrases, f, ensure_ascii=False, indent=2)
    print(f"Saved {len(phrases)} questions to {phrases_path}")
    print_stats(phrases, "phrases.json")

    print("\nGenerating collocations.json...")
    collocations = generate_collocations()
    coll_path = os.path.join(OUTPUT_DIR, "collocations.json")
    with open(coll_path, "w", encoding="utf-8") as f:
        json.dump(collocations, f, ensure_ascii=False, indent=2)
    print(f"Saved {len(collocations)} questions to {coll_path}")
    print_stats(collocations, "collocations.json")

    print("\nDone!")
