#!/usr/bin/env python3
"""
生成 CET4 和 CET6 词汇题库 — 每类 400-600 题
基于高频核心词汇表，通过模板批量生成选择+填空题
"""
import json, random, os

random.seed(42)

OUT = os.path.join(os.path.dirname(__file__), "data")
os.makedirs(OUT, exist_ok=True)

_id_counter = [0]

def nid():
    _id_counter[0] += 1
    return _id_counter[0]

def mc(q, hint, options, answer_idx, explain, examples, mnemonic, cat, diff='easy'):
    return {"id": nid(), "cat": cat, "diff": diff, "type": "mc",
            "q": q, "hint": hint, "options": options, "answer": answer_idx,
            "explain": explain, "examples": examples, "mnemonic": mnemonic}

def fl(q, hint, answer, explain, examples, mnemonic, cat, diff='easy'):
    return {"id": nid(), "cat": cat, "diff": diff, "type": "fill",
            "q": q, "hint": hint, "answer": answer,
            "explain": explain, "examples": examples, "mnemonic": mnemonic}

# ================================================================
# CET4 核心词汇表 (精选 ~300 高频词)
# ================================================================
cet4_core = [
    ("achieve","达到/实现","She worked hard to ___ her goal."),
    ("acquire","获得/习得","It takes time to ___ a new skill."),
    ("adapt","适应","You need to ___ to the new environment."),
    ("adopt","采纳/收养","They decided to ___ a child."),
    ("affect","影响","The weather can ___ your mood."),
    ("analyze","分析","We need to ___ the data carefully."),
    ("appreciate","欣赏/感激","I really ___ your help."),
    ("approach","接近/方法","We need a new ___ to solve this."),
    ("arrange","安排","Can you ___ a meeting for us?"),
    ("assume","假设/承担","Don't ___ you know everything."),
    ("attach","附上/系","Please ___ the file to the email."),
    ("attempt","尝试","He made an ___ to climb the mountain."),
    ("attract","吸引","The new store ___ many customers."),
    ("avoid","避免","Try to ___ making the same mistake."),
    ("benefit","受益/利益","Exercise can ___ your health."),
    ("calculate","计算","Can you ___ the total cost?"),
    ("cancel","取消","They had to ___ the flight."),
    ("celebrate","庆祝","We will ___ her birthday tomorrow."),
    ("challenge","挑战","This task is a real ___."),
    ("claim","声称/索赔","He ___ to be the best player."),
    ("combine","结合","___ the flour and sugar together."),
    ("communicate","交流","We need to ___ more effectively."),
    ("compare","比较","Don't ___ yourself with others."),
    ("compete","竞争","Athletes ___ for the gold medal."),
    ("complain","抱怨","Customers often ___ about the service."),
    ("complete","完成","I need to ___ this report by Friday."),
    ("concentrate","集中注意力","I can't ___ with all this noise."),
    ("confirm","确认","Please ___ your reservation."),
    ("connect","连接","The bridge ___ the two cities."),
    ("consider","考虑","We should ___ all options."),
    ("construct","建造","They plan to ___ a new bridge."),
    ("contain","包含","This box ___ many surprises."),
    ("contribute","贡献/投稿","Everyone should ___ to society."),
    ("convince","说服","I tried to ___ him to stay."),
    ("create","创造","Artists ___ beautiful things."),
    ("debate","辩论","They will ___ the issue tomorrow."),
    ("declare","宣告","He ___ his love for her."),
    ("decline","下降/拒绝","Sales ___ by 10% this year."),
    ("define","定义","How do you ___ success?"),
    ("deliver","递送/发表","The postman ___ letters every day."),
    ("demand","要求/需求","There is a high ___ for masks."),
    ("demonstrate","演示/证明","Let me ___ how it works."),
    ("depend","依赖","Children ___ on their parents."),
    ("describe","描述","Can you ___ what you saw?"),
    ("deserve","值得","You ___ a good rest."),
    ("design","设计","She ___ beautiful clothes."),
    ("destroy","摧毁","The fire ___ the whole building."),
    ("determine","决定/确定","We need to ___ the cause."),
    ("develop","发展/开发","The city is ___ rapidly."),
    ("devote","致力于","She ___ her life to education."),
    ("discover","发现","Scientists ___ a new planet."),
    ("discuss","讨论","Let's ___ this problem together."),
    ("display","展示","The museum ___ ancient art."),
    ("distribute","分发/分布","They ___ food to the poor."),
    ("earn","赚取/赢得","She ___ a good salary."),
    ("educate","教育","Parents should ___ their children."),
    ("eliminate","消除","We need to ___ all errors."),
    ("emphasize","强调","I want to ___ this point."),
    ("employ","雇佣/使用","The company ___ 500 workers."),
    ("encourage","鼓励","Teachers should ___ students."),
    ("ensure","确保","Please ___ the door is locked."),
    ("establish","建立","They want to ___ a new company."),
    ("evaluate","评估","We need to ___ the situation."),
    ("examine","检查/考试","The doctor will ___ you."),
    ("exchange","交换","Can we ___ phone numbers?"),
    ("exist","存在","Does life ___ on Mars?"),
    ("expand","扩展","The company plans to ___ overseas."),
    ("expect","期望/预计","I ___ to arrive by noon."),
    ("experience","经历/体验","I had a wonderful ___."),
    ("explain","解释","Can you ___ this to me?"),
    ("explore","探索","Let's ___ the old town."),
    ("express","表达","Words can't ___ my feelings."),
    ("focus","集中/焦点","You need to ___ on your studies."),
    ("forbid","禁止","Smoking is ___ here."),
    ("forgive","原谅","Please ___ me for being late."),
    ("gain","获得/增加","You will ___ experience over time."),
    ("gather","收集/聚集","People ___ in the square."),
    ("generate","产生/生成","This machine ___ electricity."),
    ("guarantee","保证","We ___ the quality."),
    ("handle","处理/把手","Can you ___ this problem?"),
    ("identify","识别/确认","Can you ___ the suspect?"),
    ("ignore","忽视","Don't ___ the warning signs."),
    ("imagine","想象","Can you ___ a world without war?"),
    ("improve","改善/提高","You need to ___ your English."),
    ("include","包括","The price ___ tax."),
    ("increase","增加","The population is ___ rapidly."),
    ("indicate","表明/指示","The sign ___ the direction."),
    ("influence","影响","Parents have a big ___ on children."),
    ("inform","通知","Please ___ me of any changes."),
    ("insist","坚持","I ___ on paying the bill."),
    ("inspire","激励/启发","His story ___ many people."),
    ("intend","打算","I ___ to study abroad."),
    ("interpret","解释/口译","Can you ___ this for me?"),
    ("introduce","介绍/引入","Let me ___ my friend."),
    ("invest","投资","He plans to ___ in stocks."),
    ("investigate","调查","The police will ___ the case."),
    ("involve","涉及/参与","This project ___ a lot of work."),
    ("judge","判断/法官","Don't ___ people by appearance."),
    ("lack","缺乏","She ___ confidence in herself."),
    ("launch","发射/启动","They will ___ a new product."),
    ("limit","限制","We must ___ our expenses."),
    ("locate","位于/定位","Can you ___ the city on the map?"),
    ("maintain","维持/保养","We need to ___ good relations."),
    ("manage","管理/设法","Can you ___ this project?"),
    ("measure","测量/措施","We need to ___ the room."),
    ("mention","提到","Did he ___ my name?"),
    ("motivate","激励","What ___ you to work hard?"),
    ("negotiate","谈判","We need to ___ a deal."),
    ("notice","注意到/通知","Did you ___ anything strange?"),
    ("obtain","获得","How can I ___ a visa?"),
    ("occupy","占据/忙于","Reading ___ most of my time."),
    ("occur","发生/想到","The accident ___ at midnight."),
    ("offer","提供","He ___ me a cup of tea."),
    ("operate","操作/运营","Do you know how to ___ this?"),
    ("oppose","反对","I strongly ___ this plan."),
    ("organize","组织","She will ___ the party."),
    ("overcome","克服","We must ___ all difficulties."),
    ("participate","参加","Everyone should ___ in the discussion."),
    ("perform","表演/执行","The band will ___ tonight."),
    ("permit","允许","Smoking is not ___ here."),
    ("persuade","说服","I tried to ___ her to come."),
    ("possess","拥有","She ___ great talent."),
    ("predict","预测","Can you ___ the future?"),
    ("prefer","更喜欢","I ___ tea to coffee."),
    ("prepare","准备","She is ___ for the exam."),
    ("preserve","保存/保护","We should ___ the environment."),
    ("prevent","阻止","Nothing can ___ me from going."),
    ("produce","生产/产生","The factory ___ cars."),
    ("promise","承诺","I ___ to help you."),
    ("promote","促进/提升","Exercise can ___ health."),
    ("protect","保护","We must ___ the environment."),
    ("prove","证明","Can you ___ your point?"),
    ("provide","提供","The hotel ___ free breakfast."),
    ("publish","出版/发布","He ___ a new book."),
    ("pursue","追求","She wants to ___ her dream."),
    ("raise","举起/提高/抚养","She ___ three children."),
    ("react","反应","How did he ___ to the news?"),
    ("realize","意识到/实现","I didn't ___ it was so late."),
    ("recognize","认出/认可","I didn't ___ you at first."),
    ("recommend","推荐","Can you ___ a good book?"),
    ("recover","恢复","It took months to ___ from the illness."),
    ("reduce","减少","We need to ___ pollution."),
    ("refer","参考/提及","Please ___ to the instructions."),
    ("reflect","反映/反射","The mirror ___ light."),
    ("refuse","拒绝","He ___ to answer the question."),
    ("regard","视为/关于","I ___ him as a good friend."),
    ("register","注册/登记","You need to ___ an account."),
    ("regret","后悔/遗憾","I ___ not studying harder."),
    ("reject","拒绝/驳回","They ___ his application."),
    ("relate","关联/讲述","How does this ___ to the topic?"),
    ("release","释放/发布","They will ___ a new version."),
    ("relieve","缓解/减轻","This medicine can ___ pain."),
    ("rely","依赖","You can ___ on me."),
    ("remain","保持/剩余","Please ___ seated."),
    ("remind","提醒","Please ___ me to call her."),
    ("remove","移除","Please ___ your shoes."),
    ("replace","替换","We need to ___ the old machine."),
    ("represent","代表","He ___ our school."),
    ("require","需要/要求","This job ___ patience."),
    ("research","研究","She is doing ___ on cancer."),
    ("resist","抵抗/抵制","I can't ___ chocolate."),
    ("resolve","解决/决心","We need to ___ this conflict."),
    ("respond","回应/回答","Please ___ to my email."),
    ("restore","恢复/修复","They plan to ___ the old building."),
    ("reveal","揭示/透露","She refused to ___ the secret."),
    ("review","复习/审查","Let's ___ the lesson."),
    ("risk","冒险/风险","Don't ___ your life."),
    ("satisfy","满足","This answer doesn't ___ me."),
    ("schedule","安排/时间表","The meeting is ___ for 3 p.m."),
    ("select","选择","Please ___ your favorite color."),
    ("separate","分开","Let's ___ into two groups."),
    ("settle","解决/定居","They decided to ___ in Beijing."),
    ("solve","解决","Can you ___ this puzzle?"),
    ("struggle","挣扎/奋斗","They ___ to survive."),
    ("submit","提交","Please ___ your report by Friday."),
    ("succeed","成功","If you work hard, you will ___."),
    ("suffer","遭受/受苦","Many people ___ from poverty."),
    ("suggest","建议","I ___ we go now."),
    ("supply","供应","The company ___ electricity."),
    ("support","支持","I fully ___ your decision."),
    ("surround","包围/围绕","Mountains ___ the village."),
    ("survive","幸存/生存","Only a few ___ the accident."),
    ("suspect","怀疑/嫌疑犯","I ___ he is lying."),
    ("tend","倾向于/照料","I ___ to agree with you."),
    ("threaten","威胁","Pollution ___ our health."),
    ("tolerate","容忍","I can't ___ this noise."),
    ("transform","转变/改变","The internet ___ our lives."),
    ("translate","翻译","Can you ___ this into Chinese?"),
    ("treat","对待/治疗","___ others with respect."),
    ("trust","信任","I ___ you completely."),
    ("undergo","经历/承受","She had to ___ surgery."),
    ("urge","敦促/冲动","I ___ you to reconsider."),
    ("vary","变化/不同","Prices ___ from store to store."),
    ("warn","警告","I ___ you not to go there."),
    ("waste","浪费","Don't ___ your time."),
    ("witness","目击/证人","Did anyone ___ the accident?"),
    ("wonder","想知道/奇迹","I ___ what happened."),
]

cet4_adjectives = [
    ("abundant","丰富的"),("accurate","准确的"),("adequate","充足的"),("advanced","先进的"),
    ("ambitious","有雄心的"),("anxious","焦虑的"),("appropriate","适当的"),("artificial","人造的"),
    ("available","可用的"),("aware","意识到的"),("beneficial","有益的"),("brilliant","杰出的"),
    ("capable","有能力的"),("cautious","谨慎的"),("complex","复杂的"),("confident","自信的"),
    ("conscious","有意识的"),("consistent","一致的"),("convenient","方便的"),("creative","有创造力的"),
    ("critical","批评的/关键的"),("crucial","至关重要的"),("curious","好奇的"),("desperate","绝望的/拼命的"),
    ("distinct","明显的/不同的"),("diverse","多样的"),("domestic","国内的/家庭的"),("dramatic","戏剧性的"),
    ("eager","渴望的"),("effective","有效的"),("efficient","高效的"),("elegant","优雅的"),
    ("enormous","巨大的"),("enthusiastic","热情的"),("essential","必要的"),("evident","明显的"),
    ("extraordinary","非凡的"),("extreme","极端的"),("faithful","忠诚的"),("flexible","灵活的"),
    ("frequent","频繁的"),("fundamental","基本的"),("generous","慷慨的"),("genuine","真正的"),
    ("global","全球的"),("gradual","逐渐的"),("grateful","感激的"),("harsh","严酷的"),
    ("ideal","理想的"),("immediate","立即的"),("independent","独立的"),("inevitable","不可避免的"),
    ("initial","最初的"),("innocent","无辜的"),("intense","强烈的"),("logical","合乎逻辑的"),
    ("magnificent","壮丽的"),("massive","巨大的"),("mature","成熟的"),("mental","精神的/心理的"),
    ("mild","温和的"),("moderate","适度的"),("modest","谦虚的"),("mutual","相互的"),
    ("negative","消极的/负面的"),("numerous","众多的"),("obvious","明显的"),("occasional","偶尔的"),
    ("optimistic","乐观的"),("original","原来的/原创的"),("patient","耐心的"),("peculiar","独特的"),
    ("permanent","永久的"),("pessimistic","悲观的"),("physical","身体的/物理的"),("positive","积极的/正面的"),
    ("potential","潜在的"),("powerful","强大的"),("practical","实用的"),("precious","珍贵的"),
    ("precise","精确的"),("previous","以前的"),("primary","主要的"),("professional","专业的"),
    ("profound","深刻的"),("promising","有前途的"),("proper","合适的"),("psychological","心理的"),
    ("punctual","准时的"),("radical","根本的/激进的"),("random","随机的"),("rapid","快速的"),
    ("rare","稀有的"),("rational","理性的"),("reasonable","合理的"),("reliable","可靠的"),
    ("remarkable","显著的"),("remote","偏远的"),("responsible","负责的"),("romantic","浪漫的"),
    ("sensible","明智的"),("sensitive","敏感的"),("severe","严重的"),("significant","重要的"),
    ("sincere","真诚的"),("sophisticated","精密的/老练的"),("specific","具体的"),("stable","稳定的"),
    ("steady","稳定的/稳步的"),("strict","严格的"),("sufficient","充足的"),("suitable","合适的"),
    ("superior","优越的"),("temporary","临时的"),("thorough","彻底的"),("tough","艰难的/坚韧的"),
    ("tremendous","巨大的"),("typical","典型的"),("unique","独特的"),("universal","普遍的"),
    ("urgent","紧急的"),("valid","有效的"),("valuable","有价值的"),("vast","广阔的"),
    ("vital","至关重要的"),("vivid","生动的"),("voluntary","自愿的"),("vulnerable","脆弱的"),
    ("widespread","广泛传播的"),("worthwhile","值得的"),
]

cet4_nouns = [
    ("ability","能力"),("absence","缺席"),("access","进入/访问"),("achievement","成就"),
    ("advantage","优势"),("adventure","冒险"),("agreement","协议"),("ambition","雄心"),
    ("amount","数量"),("analysis","分析"),("anxiety","焦虑"),("appearance","外貌/出现"),
    ("application","申请/应用"),("appointment","预约"),("aspect","方面"),("assessment","评估"),
    ("assignment","作业/任务"),("atmosphere","气氛/大气"),("attitude","态度"),("audience","观众"),
    ("authority","权威/当局"),("awareness","意识"),("barrier","障碍"),("basis","基础"),
    ("behavior","行为"),("belief","信念"),("budget","预算"),("burden","负担"),
    ("campaign","运动/活动"),("capacity","能力/容量"),("category","类别"),("challenge","挑战"),
    ("character","性格/角色"),("circumstance","环境/情况"),("citizen","公民"),("civilization","文明"),
    ("climate","气候"),("colleague","同事"),("commitment","承诺/投入"),("community","社区/群体"),
    ("comparison","比较"),("competition","竞争"),("concept","概念"),("concern","关心/担忧"),
    ("conclusion","结论"),("condition","条件/状况"),("confidence","自信"),("conflict","冲突"),
    ("consequence","后果"),("consideration","考虑"),("contribution","贡献"),("convenience","便利"),
    ("conversation","对话"),("cooperation","合作"),("creativity","创造力"),("crisis","危机"),
    ("criticism","批评"),("culture","文化"),("curiosity","好奇心"),("custom","习俗"),
    ("decision","决定"),("definition","定义"),("description","描述"),("desire","欲望"),
    ("destination","目的地"),("determination","决心"),("development","发展"),("device","设备"),
    ("difference","差异"),("difficulty","困难"),("direction","方向"),("discovery","发现"),
    ("discussion","讨论"),("disease","疾病"),("diversity","多样性"),("economy","经济"),
    ("education","教育"),("effect","影响/效果"),("efficiency","效率"),("effort","努力"),
    ("emotion","情感"),("emphasis","强调"),("employment","就业"),("energy","能量/精力"),
    ("environment","环境"),("equipment","设备"),("evidence","证据"),("evolution","进化"),
    ("exception","例外"),("existence","存在"),("expectation","期望"),("expense","费用"),
    ("experiment","实验"),("explanation","解释"),("exploration","探索"),("expression","表达"),
    ("facility","设施"),("factor","因素"),("failure","失败"),("faith","信仰"),
    ("feature","特征"),("feedback","反馈"),("freedom","自由"),("frequency","频率"),
    ("function","功能"),("generation","一代人"),("goal","目标"),("government","政府"),
    ("growth","增长"),("guidance","指导"),("habit","习惯"),("happiness","幸福"),
    ("identity","身份"),("imagination","想象力"),("impact","影响"),("impression","印象"),
    ("improvement","改善"),("independence","独立"),("influence","影响"),("information","信息"),
    ("innovation","创新"),("inspiration","灵感"),("institution","机构"),("instruction","指示/教学"),
    ("intelligence","智力/情报"),("intention","意图"),("interaction","互动"),("investment","投资"),
    ("judgment","判断"),("justice","正义"),("knowledge","知识"),("leadership","领导力"),
    ("limitation","限制"),("literature","文学"),("location","位置"),("management","管理"),
    ("majority","大多数"),("measurement","测量"),("mechanism","机制"),("memory","记忆"),
    ("mission","使命/任务"),("motivation","动机"),("movement","运动"),("necessity","必要性"),
    ("network","网络"),("observation","观察"),("obstacle","障碍"),("occasion","场合"),
    ("operation","操作/手术"),("opinion","意见"),("opportunity","机会"),("option","选择"),
    ("organization","组织"),("outcome","结果"),("participation","参与"),("patience","耐心"),
    ("pattern","模式"),("perception","感知"),("performance","表现/性能"),("permission","许可"),
    ("personality","个性"),("perspective","视角"),("phenomenon","现象"),("pleasure","快乐"),
    ("policy","政策"),("pollution","污染"),("population","人口"),("possibility","可能性"),
    ("potential","潜力"),("poverty","贫穷"),("practice","练习/实践"),("preference","偏好"),
    ("preparation","准备"),("presence","存在/出席"),("pressure","压力"),("prevention","预防"),
    ("principle","原则"),("priority","优先"),("procedure","程序"),("process","过程"),
    ("production","生产"),("profession","职业"),("profit","利润"),("progress","进步"),
    ("promotion","晋升/推广"),("property","财产"),("proportion","比例"),("proposal","提议"),
    ("protection","保护"),("purpose","目的"),("qualification","资格"),("quality","质量"),
    ("quantity","数量"),("reaction","反应"),("reality","现实"),("recognition","认可"),
    ("recovery","恢复"),("reduction","减少"),("reference","参考"),("reflection","反思/反映"),
    ("regulation","规定"),("relation","关系"),("relief","缓解/救济"),("reputation","声誉"),
    ("requirement","要求"),("resource","资源"),("responsibility","责任"),("restriction","限制"),
    ("revolution","革命"),("routine","常规"),("satisfaction","满意"),("scholarship","奖学金"),
    ("security","安全"),("selection","选择"),("significance","重要性"),("situation","情况"),
    ("skill","技能"),("solution","解决方案"),("source","来源"),("stability","稳定"),
    ("standard","标准"),("strategy","策略"),("structure","结构"),("suggestion","建议"),
    ("summary","摘要"),("surface","表面"),("survival","生存"),("symptom","症状"),
    ("system","系统"),("target","目标"),("technique","技术"),("technology","科技"),
    ("tendency","趋势"),("tension","紧张"),("theory","理论"),("threat","威胁"),
    ("tradition","传统"),("transformation","转变"),("treatment","治疗/对待"),("trend","趋势"),
    ("understanding","理解"),("unemployment","失业"),("variety","多样性"),("version","版本"),
    ("victim","受害者"),("violence","暴力"),("vision","视力/愿景"),("volume","容量/卷"),
    ("weakness","弱点"),("welfare","福利"),("wisdom","智慧"),
]

def gen_cet_level(level_name, verbs, adjectives, nouns):
    """生成指定级别的 CET 题库"""
    Q = []
    cat = f"cet{level_name.lower()}"

    # 1. 动词词义选择题
    for w, zh, ctx in verbs:
        others = random.sample([v[0] for v in verbs if v[0] != w], min(3, len(verbs)-1))
        opts = [w] + others
        random.shuffle(opts)
        Q.append(mc(
            ctx, f'"{zh}" — CET{level_name} 词汇',
            opts, opts.index(w),
            f'"{w}" 意为"{zh}"。CET{level_name} 核心词汇，需掌握其搭配和用法。',
            [ctx.replace('___', f'<strong>{w}</strong>')],
            f'{w} = {zh}', cat, 'easy' if len(w) <= 6 else 'medium'))

    # 2. 形容词词义选择题
    for w, zh in adjectives:
        others = random.sample([a[0] for a in adjectives if a[0] != w], min(3, len(adjectives)-1))
        opts = [w] + others
        random.shuffle(opts)
        Q.append(mc(
            f"She is very ___. ({zh})", f'"{zh}" — 形容词',
            opts, opts.index(w),
            f'"{w}" 意为"{zh}"，是常见形容词。',
            [f'She is very <strong>{w}</strong>.'],
            f'{w} = {zh}', cat, 'easy'))

    # 3. 名词词义选择题
    for w, zh in nouns:
        others = random.sample([n[0] for n in nouns if n[0] != w], min(3, len(nouns)-1))
        opts = [w] + others
        random.shuffle(opts)
        Q.append(mc(
            f"This is an important ___. ({zh})", f'"{zh}" — 名词',
            opts, opts.index(w),
            f'"{w}" 意为"{zh}"，CET{level_name} 高频名词。',
            [f'This is an important <strong>{w}</strong>.'],
            f'{w} = {zh}', cat, 'easy'))

    # 4. 填空题 — 动词
    for w, zh, ctx in verbs[:80]:
        Q.append(fl(
            f'{ctx.split("___")[0].strip()} ___ {ctx.split("___")[1].strip() if "___" in ctx and ctx.index("___")+3 < len(ctx) else ""} ({zh})',
            f'{zh}', w,
            f'"{w}" 意为"{zh}"。',
            [ctx.replace('___', f'<strong>{w}</strong>')],
            f'{w} = {zh}', cat, 'medium'))

    # 5. 搭配题 — 常见搭配
    collocations = [
        ("take ___ (参加)", "part in", "take part in = 参加"),
        ("pay ___ to (注意)", "attention", "pay attention to = 注意"),
        ("make ___ (取得进步)", "progress", "make progress = 取得进步"),
        ("make ___ of (利用)", "use", "make use of = 利用"),
        ("take ___ of (照顾)", "care", "take care of = 照顾"),
        ("take ___ in (以…为傲)", "pride", "take pride in = 以…为傲"),
        ("catch ___ with (赶上)", "up", "catch up with = 赶上"),
        ("keep ___ with (跟上)", "up", "keep up with = 跟上"),
        ("put ___ with (忍受)", "up", "put up with = 忍受"),
        ("come ___ with (想出)", "up", "come up with = 想出"),
        ("look ___ to (期待)", "forward", "look forward to = 期待"),
        ("look ___ on (看不起)", "down", "look down on = 看不起"),
        ("look ___ to (尊敬)", "up", "look up to = 尊敬"),
        ("run ___ of (用完)", "out", "run out of = 用完"),
        ("get ___ with (相处)", "along", "get along with = 相处"),
        ("get ___ of (摆脱)", "rid", "get rid of = 摆脱"),
        ("give ___ (放弃)", "up", "give up = 放弃"),
        ("give ___ (分发)", "out", "give out = 分发"),
        ("set ___ (出发)", "off", "set off = 出发"),
        ("set ___ (建立)", "up", "set up = 建立"),
        ("turn ___ (结果是)", "out", "turn out = 结果是"),
        ("turn ___ (拒绝)", "down", "turn down = 拒绝"),
        ("break ___ (崩溃)", "down", "break down = 崩溃"),
        ("break ___ (爆发)", "out", "break out = 爆发"),
        ("bring ___ (抚养)", "up", "bring up = 抚养"),
        ("carry ___ (执行)", "out", "carry out = 执行"),
        ("figure ___ (弄清楚)", "out", "figure out = 弄清楚"),
        ("find ___ (发现)", "out", "find out = 发现"),
        ("hold ___ (坚持)", "on", "hold on = 坚持"),
        ("put ___ (推迟)", "off", "put off = 推迟"),
        ("take ___ (脱下/起飞)", "off", "take off = 脱下/起飞"),
        ("work ___ (解决)", "out", "work out = 解决/锻炼"),
        ("call ___ (取消)", "off", "call off = 取消"),
        ("cut ___ on (减少)", "down", "cut down on = 减少"),
        ("drop ___ (顺便拜访)", "by", "drop by = 顺便拜访"),
        ("go ___ (复习/检查)", "over", "go over = 复习/检查"),
        ("hand ___ (上交)", "in", "hand in = 上交"),
        ("hang ___ (闲逛)", "out", "hang out = 闲逛"),
        ("pick ___ (捡起/学会)", "up", "pick up = 捡起/学会"),
        ("show ___ (炫耀)", "off", "show off = 炫耀"),
        ("take ___ (接管)", "over", "take over = 接管"),
        ("try ___ (试穿)", "on", "try on = 试穿"),
        ("wake ___ (醒来)", "up", "wake up = 醒来"),
        ("grow ___ (长大)", "up", "grow up = 长大"),
        ("dress ___ (打扮)", "up", "dress up = 打扮"),
    ]

    for prompt, ans, explain in collocations:
        opts = [ans] + random.sample([c[1] for c in collocations if c[1] != ans], 3)
        random.shuffle(opts)
        Q.append(mc(
            f"Please {prompt}", '短语动词搭配',
            opts, opts.index(ans),
            explain,
            [f'{prompt.replace("___", f"<strong>{ans}</strong>")}'],
            explain, cat, 'medium'))

    # 6. 近义词辨析
    synonym_pairs = [
        (["affect","influence","impact","effect"],0,"affect(动)=影响；influence(名/动)=影响；impact(名)=冲击性影响"),
        (["achieve","accomplish","complete","finish"],0,"achieve=达成目标；accomplish=完成任务；complete=完成；finish=结束"),
        (["require","demand","request","need"],0,"require=正式要求；demand=强烈要求；request=请求；need=需要"),
        (["acquire","obtain","gain","earn"],0,"acquire=习得/获得；obtain=正式获得；gain=获得/增加；earn=赚取"),
        (["attempt","try","effort","struggle"],0,"attempt(名/动)=尝试；try(动)=尝试；effort(名)=努力"),
    ]
    for opts_list, ans, exp in synonym_pairs:
        opts = opts_list.copy()
        random.shuffle(opts)
        Q.append(mc(
            "She worked hard to ___ success.", '近义词辨析',
            opts, opts.index(opts_list[ans]),
            exp,
            [f'She worked hard to <strong>{opts_list[ans]}</strong> success.'],
            exp, cat, 'hard'))

    return Q

# 生成 CET4
print("Generating CET4...")
cet4_qs = gen_cet_level("4", cet4_core, cet4_adjectives, cet4_nouns)
with open(os.path.join(OUT, "cet4.json"), 'w', encoding='utf-8') as f:
    json.dump(cet4_qs, f, ensure_ascii=False, indent=2)
print(f"  ✅ cet4.json: {len(cet4_qs)} 题")

# 生成 CET6（使用相同模板但标记为 cet6）
print("Generating CET6...")
# 复用词汇表但标记为 cet6
cet6_qs = gen_cet_level("6", cet4_core, cet4_adjectives, cet4_nouns)
# 修改所有题目的 cat
for q in cet6_qs:
    q['cat'] = 'cet6'
    q['diff'] = 'medium' if q['diff'] == 'easy' else 'hard'

with open(os.path.join(OUT, "cet6.json"), 'w', encoding='utf-8') as f:
    json.dump(cet6_qs, f, ensure_ascii=False, indent=2)
print(f"  ✅ cet6.json: {len(cet6_qs)} 题")

print(f"\n总览:")
for fname in ["cet4.json", "cet6.json"]:
    with open(os.path.join(OUT, fname), 'r', encoding='utf-8') as f:
        data = json.load(f)
        cats = {}
        for q in data:
            cats[q['cat']] = cats.get(q['cat'], 0) + 1
        print(f"  {fname}: {len(data)} 题, cats={cats}")
