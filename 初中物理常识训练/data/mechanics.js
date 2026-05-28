const MECHANICS_QUESTIONS = [
  {
    "id": 1,
    "type": "choice",
    "question": "下列关于机械运动的说法正确的是",
    "options": [
      "A. 只有机器的运动才是机械运动",
      "B. 物体位置的改变叫做机械运动",
      "C. 机械运动就是速度很大的运动",
      "D. 微粒的运动是机械运动"
    ],
    "answer": "B",
    "explanation": "机械运动是指物体位置的变化，与速度大小无关"
  },
  {
    "id": 2,
    "type": "choice",
    "question": "下列现象中不属于机械运动的是",
    "options": [
      "A. 地球绕太阳公转",
      "B. 铁锅生锈",
      "C. 小鸟飞翔",
      "D. 汽车行驶"
    ],
    "answer": "B",
    "explanation": "铁锅生锈是化学变化，不是位置变化"
  },
  {
    "id": 3,
    "type": "fill",
    "question": "研究物体运动时，被选作标准的物体叫做___。",
    "options": [],
    "answer": "参照物",
    "explanation": "判断物体是否运动需要选择参照物"
  },
  {
    "id": 4,
    "type": "choice",
    "question": "坐在行驶的客车里的乘客，以客车为参照物，路边的树木是",
    "options": [
      "A. 静止的",
      "B. 向前运动的",
      "C. 向后运动的",
      "D. 无法判断"
    ],
    "answer": "C",
    "explanation": "以客车为参照物，路边树木相对客车向后运动"
  },
  {
    "id": 5,
    "type": "fill",
    "question": "甲乙两人并排同行，以地面为参照物甲是运动的；以乙为参照物，甲是___的。",
    "options": [],
    "answer": "静止",
    "explanation": "甲乙速度相同相对位置不变"
  },
  {
    "id": 6,
    "type": "choice",
    "question": "下列关于速度的说法正确的是",
    "options": [
      "A. 速度是描述物体运动快慢的物理量",
      "B. 速度越大距离越远",
      "C. 速度越大时间越短",
      "D. 用时越少速度越小"
    ],
    "answer": "A",
    "explanation": "速度是描述物体运动快慢的物理量"
  },
  {
    "id": 7,
    "type": "fill",
    "question": "速度的公式是v=___。",
    "options": [],
    "answer": "s/t",
    "explanation": "速度等于路程除以时间"
  },
  {
    "id": 8,
    "type": "choice",
    "question": "在国际单位制中，速度的单位是",
    "options": [
      "A. km/h",
      "B. m/s",
      "C. cm/s",
      "D. m/min"
    ],
    "answer": "B",
    "explanation": "国际单位制中速度单位是m/s"
  },
  {
    "id": 9,
    "type": "fill",
    "question": "54km/h=___m/s。",
    "options": [],
    "answer": "15",
    "explanation": "54÷3.6=15m/s"
  },
  {
    "id": 10,
    "type": "choice",
    "question": "一辆汽车前2h行驶80km，后3h行驶120km，全程平均速度是",
    "options": [
      "A. 40km/h",
      "B. 45km/h",
      "C. 48km/h",
      "D. 50km/h"
    ],
    "answer": "A",
    "explanation": "总路程200km/总时间5h=40km/h"
  },
  {
    "id": 11,
    "type": "fill",
    "question": "以20m/s匀速行驶30s通过的路程是___m。",
    "options": [],
    "answer": "600",
    "explanation": "s=vt=20×30=600m"
  },
  {
    "id": 12,
    "type": "choice",
    "question": "甲乙路程之比2:3，时间之比1:2，速度之比为",
    "options": [
      "A. 1:3",
      "B. 4:3",
      "C. 3:1",
      "D. 3:4"
    ],
    "answer": "B",
    "explanation": "v甲:v乙=(2/1):(3/2)=4:3"
  },
  {
    "id": 13,
    "type": "choice",
    "question": "下列属于匀速直线运动的是",
    "options": [
      "A. 自由下落的石块",
      "B. 沿平直轨道匀速行驶的列车",
      "C. 绕操场匀速跑步",
      "D. 正在进站的火车"
    ],
    "answer": "B",
    "explanation": "匀速直线运动要求速度和方向都不变"
  },
  {
    "id": 14,
    "type": "fill",
    "question": "人正常步行的速度大约是___m/s。",
    "options": [],
    "answer": "1.1",
    "explanation": "正常步行约为1.1m/s"
  },
  {
    "id": 15,
    "type": "choice",
    "question": "甲车速度72km/h，乙车速度20m/s，则",
    "options": [
      "A. 甲车快",
      "B. 乙车快",
      "C. 一样快",
      "D. 无法比较"
    ],
    "answer": "C",
    "explanation": "72km/h=20m/s，相同"
  },
  {
    "id": 16,
    "type": "fill",
    "question": "百米赛跑用了12.5s，平均速度是___m/s。",
    "options": [],
    "answer": "8",
    "explanation": "v=100/12.5=8m/s"
  },
  {
    "id": 17,
    "type": "choice",
    "question": "关于平均速度正确的是",
    "options": [
      "A. 平均速度是速度的平均值",
      "B. 平均速度能精确描述变速运动",
      "C. 平均速度=总路程/总时间",
      "D. 匀速直线运动没有平均速度"
    ],
    "answer": "C",
    "explanation": "平均速度=总路程/总时间"
  },
  {
    "id": 18,
    "type": "fill",
    "question": "前半程40km/h后半程60km/h，全程平均速度为___km/h。",
    "options": [],
    "answer": "48",
    "explanation": "设总路程2s，平均速度=2s/(s/40+s/60)=48km/h"
  },
  {
    "id": 19,
    "type": "choice",
    "question": "跑两圈比跑第一圈平均速度",
    "options": [
      "A. 变大",
      "B. 变小",
      "C. 不变",
      "D. 无法判断"
    ],
    "answer": "B",
    "explanation": "总时间更长平均速度变小"
  },
  {
    "id": 20,
    "type": "fill",
    "question": "匀速直线运动中速度的大小和方向都___。",
    "options": [],
    "answer": "不变",
    "explanation": "匀速直线运动速度大小和方向都不变"
  },
  {
    "id": 21,
    "type": "choice",
    "question": "关于运动和静止正确的是",
    "options": [
      "A. 运动是绝对的静止是相对的",
      "B. 静止是绝对的运动是相对的",
      "C. 都是绝对的",
      "D. 都是相对的"
    ],
    "answer": "A",
    "explanation": "运动是绝对的，静止是相对的"
  },
  {
    "id": 22,
    "type": "choice",
    "question": "s-t图像中直线越陡速度越大，甲比乙陡则",
    "options": [
      "A. v甲>v乙",
      "B. v甲<v乙",
      "C. v甲=v乙",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "图线越陡速度越大"
  },
  {
    "id": 23,
    "type": "fill",
    "question": "声音在空气中的传播速度约为___m/s。",
    "options": [],
    "answer": "340",
    "explanation": "声音在空气中约340m/s"
  },
  {
    "id": 24,
    "type": "choice",
    "question": "看到闪电后5s听到雷声，距雷电大约",
    "options": [
      "A. 1500m",
      "B. 1700m",
      "C. 2000m",
      "D. 1000m"
    ],
    "answer": "B",
    "explanation": "s=340×5=1700m"
  },
  {
    "id": 25,
    "type": "fill",
    "question": "一般选择___为参照物最方便。",
    "options": [],
    "answer": "地面",
    "explanation": "一般选择地面为参照物"
  },
  {
    "id": 26,
    "type": "choice",
    "question": "关于质量的说法正确的是",
    "options": [
      "A. 压扁质量变小",
      "B. 带到月球质量变小",
      "C. 熔化成铁水质量不变",
      "D. 磨成铁粉质量变小"
    ],
    "answer": "C",
    "explanation": "质量不随形状状态位置变化"
  },
  {
    "id": 27,
    "type": "fill",
    "question": "物体所含___的多少叫做质量。",
    "options": [],
    "answer": "物质",
    "explanation": "质量的定义"
  },
  {
    "id": 28,
    "type": "choice",
    "question": "国际单位制中质量的单位是",
    "options": [
      "A. g",
      "B. kg",
      "C. t",
      "D. mg"
    ],
    "answer": "B",
    "explanation": "国际单位制中质量单位是千克"
  },
  {
    "id": 29,
    "type": "fill",
    "question": "一个中学生质量大约___kg。",
    "options": [],
    "answer": "50",
    "explanation": "普通中学生约50kg"
  },
  {
    "id": 30,
    "type": "choice",
    "question": "砝码磨损后测量结果",
    "options": [
      "A. 偏大",
      "B. 偏小",
      "C. 不变",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "砝码磨损后需更多砝码平衡测量值偏大"
  },
  {
    "id": 31,
    "type": "fill",
    "question": "使用天平应放在___台上。",
    "options": [],
    "answer": "水平",
    "explanation": "天平应放在水平工作台上"
  },
  {
    "id": 32,
    "type": "choice",
    "question": "被测物体应放在天平的",
    "options": [
      "A. 左盘",
      "B. 右盘",
      "C. 都可以",
      "D. 随意"
    ],
    "answer": "A",
    "explanation": "左物右码"
  },
  {
    "id": 33,
    "type": "fill",
    "question": "质量与体积之比叫做物质的___。",
    "options": [],
    "answer": "密度",
    "explanation": "密度的定义"
  },
  {
    "id": 34,
    "type": "choice",
    "question": "密度的计算公式是",
    "options": [
      "A. ρ=mV",
      "B. ρ=m/V",
      "C. ρ=V/m",
      "D. ρ=m²/V"
    ],
    "answer": "B",
    "explanation": "密度等于质量除以体积"
  },
  {
    "id": 35,
    "type": "fill",
    "question": "水的密度是___kg/m³。",
    "options": [],
    "answer": "1.0×10³",
    "explanation": "水的密度为1.0×10³kg/m³"
  },
  {
    "id": 36,
    "type": "choice",
    "question": "铁的质量7.9kg体积1×10⁻³m³，铁的密度为",
    "options": [
      "A. 7.9kg/m³",
      "B. 7.9×10³kg/m³",
      "C. 7.9g/cm³",
      "D. 79×10³kg/m³"
    ],
    "answer": "B",
    "explanation": "ρ=7.9/(1×10⁻³)=7.9×10³kg/m³"
  },
  {
    "id": 37,
    "type": "fill",
    "question": "1g/cm³=___kg/m³。",
    "options": [],
    "answer": "1000",
    "explanation": "1g/cm³=1000kg/m³"
  },
  {
    "id": 38,
    "type": "choice",
    "question": "关于密度正确的是",
    "options": [
      "A. 密度与质量成正比",
      "B. 密度与体积成反比",
      "C. 密度是物质特性与质量体积无关",
      "D. 切去一半密度也变半"
    ],
    "answer": "C",
    "explanation": "密度是物质特性与质量体积无关"
  },
  {
    "id": 39,
    "type": "fill",
    "question": "冰熔化成水后质量___。",
    "options": [],
    "answer": "不变",
    "explanation": "状态变化质量不变"
  },
  {
    "id": 40,
    "type": "choice",
    "question": "冰熔化成水后密度将",
    "options": [
      "A. 变大",
      "B. 变小",
      "C. 不变",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "水密度大于冰的密度"
  },
  {
    "id": 41,
    "type": "fill",
    "question": "瓶子最多装1kg水其容积是___mL。",
    "options": [],
    "answer": "1000",
    "explanation": "V=1/1.0×10³=1000mL"
  },
  {
    "id": 42,
    "type": "choice",
    "question": "装1kg水的瓶子装酒精最多（ρ酒精=0.8×10³kg/m³）",
    "options": [
      "A. 1kg",
      "B. 0.8kg",
      "C. 1.25kg",
      "D. 0.64kg"
    ],
    "answer": "B",
    "explanation": "容积不变m=ρV=0.8kg"
  },
  {
    "id": 43,
    "type": "fill",
    "question": "量筒是测量___的仪器。",
    "options": [],
    "answer": "体积",
    "explanation": "量筒测量液体体积"
  },
  {
    "id": 44,
    "type": "choice",
    "question": "量筒读数视线应与液面",
    "options": [
      "A. 最高处相平",
      "B. 凹面最低处相平",
      "C. 凸面最高处相平",
      "D. 任意位置"
    ],
    "answer": "B",
    "explanation": "视线应与凹面最低处相平"
  },
  {
    "id": 45,
    "type": "fill",
    "question": "鉴别物质可根据___来判定。",
    "options": [],
    "answer": "密度",
    "explanation": "密度是物质特性可用于鉴别"
  },
  {
    "id": 46,
    "type": "choice",
    "question": "质量相同体积之比3:2，密度之比为",
    "options": [
      "A. 3:2",
      "B. 2:3",
      "C. 1:1",
      "D. 9:4"
    ],
    "answer": "B",
    "explanation": "质量相同密度与体积成反比"
  },
  {
    "id": 47,
    "type": "fill",
    "question": "铁球158g体积40cm³，该铁球是___心的。（ρ铁=7.9g/cm³）",
    "options": [],
    "answer": "空",
    "explanation": "实心体积应为20cm³小于40cm³"
  },
  {
    "id": 48,
    "type": "choice",
    "question": "同质量的水酒精硫酸液面最高的是",
    "options": [
      "A. 水",
      "B. 酒精",
      "C. 硫酸",
      "D. 一样高"
    ],
    "answer": "B",
    "explanation": "密度最小体积最大液面最高"
  },
  {
    "id": 49,
    "type": "fill",
    "question": "金属10.8g体积4cm³，密度是___g/cm³。",
    "options": [],
    "answer": "2.7",
    "explanation": "ρ=10.8/4=2.7g/cm³"
  },
  {
    "id": 50,
    "type": "choice",
    "question": "金属带到月球上质量和密度",
    "options": [
      "A. 都变小",
      "B. 都不变",
      "C. 质量变小密度不变",
      "D. 质量不变密度变小"
    ],
    "answer": "B",
    "explanation": "质量和密度都不随位置变化"
  },
  {
    "id": 51,
    "type": "choice",
    "question": "关于力正确的是",
    "options": [
      "A. 力是物体本身具有的",
      "B. 力可以脱离物体存在",
      "C. 力是物体对物体的作用",
      "D. 只有接触才能产生力"
    ],
    "answer": "C",
    "explanation": "力是物体对物体的作用，磁力等不需要接触"
  },
  {
    "id": 52,
    "type": "fill",
    "question": "力是___对___的作用。",
    "options": [],
    "answer": "物体；物体",
    "explanation": "力是物体对物体的作用"
  },
  {
    "id": 53,
    "type": "choice",
    "question": "关于力说法错误的是",
    "options": [
      "A. 力的作用是相互的",
      "B. 一个物体也能产生力的作用",
      "C. 施力物体同时也是受力物体",
      "D. 物体间力的作用同时发生"
    ],
    "answer": "B",
    "explanation": "一个物体不能产生力的作用"
  },
  {
    "id": 54,
    "type": "fill",
    "question": "物体间力的作用是___的。",
    "options": [],
    "answer": "相互",
    "explanation": "物体间力的作用是相互的"
  },
  {
    "id": 55,
    "type": "choice",
    "question": "游泳时使人前进的力是",
    "options": [
      "A. 人对水的力",
      "B. 水对人的力",
      "C. 人本身的冲力",
      "D. 水的浮力"
    ],
    "answer": "B",
    "explanation": "水对人的反作用力推动人前进"
  },
  {
    "id": 56,
    "type": "fill",
    "question": "力的三要素是力的大小、___和作用点。",
    "options": [],
    "answer": "方向",
    "explanation": "力的三要素：大小、方向、作用点"
  },
  {
    "id": 57,
    "type": "choice",
    "question": "力的作用效果是",
    "options": [
      "A. 只改变形状",
      "B. 只改变运动状态",
      "C. 改变形状或改变运动状态",
      "D. 只改变运动方向"
    ],
    "answer": "C",
    "explanation": "力的作用效果有两个"
  },
  {
    "id": 58,
    "type": "fill",
    "question": "运动状态的改变包括速度大小的改变和___的改变。",
    "options": [],
    "answer": "运动方向",
    "explanation": "运动状态改变包括速度和方向"
  },
  {
    "id": 59,
    "type": "choice",
    "question": "说明力改变了物体形状的是",
    "options": [
      "A. 推车由静止变为运动",
      "B. 拉弓弓被拉弯",
      "C. 踢球球飞出",
      "D. 推门门被推开"
    ],
    "answer": "B",
    "explanation": "弓被拉弯是形状改变"
  },
  {
    "id": 60,
    "type": "fill",
    "question": "推门时推力离门轴越远门越容易推开，说明力的作用效果与力的___有关。",
    "options": [],
    "answer": "作用点",
    "explanation": "作用点不同效果不同"
  },
  {
    "id": 61,
    "type": "choice",
    "question": "力的国际单位是",
    "options": [
      "A. 千克",
      "B. 牛顿",
      "C. 帕斯卡",
      "D. 焦耳"
    ],
    "answer": "B",
    "explanation": "力的国际单位是牛顿"
  },
  {
    "id": 62,
    "type": "fill",
    "question": "拉弹簧弹簧伸长，说明力可以使物体发生___。",
    "options": [],
    "answer": "形变",
    "explanation": "弹簧伸长是形状改变即形变"
  },
  {
    "id": 63,
    "type": "choice",
    "question": "关于力的三要素正确的是",
    "options": [
      "A. 只要知道大小就能确定效果",
      "B. 任一改变效果就改变",
      "C. 只改变大小效果不变",
      "D. 三要素不变效果也可能不变"
    ],
    "answer": "B",
    "explanation": "三要素任一改变效果就改变"
  },
  {
    "id": 64,
    "type": "fill",
    "question": "踢足球时脚感到疼，说明物体间力的作用是___的。",
    "options": [],
    "answer": "相互",
    "explanation": "足球对脚也施加了力的作用"
  },
  {
    "id": 65,
    "type": "choice",
    "question": "属于力改变运动方向的是",
    "options": [
      "A. 踢球球由静止变为运动",
      "B. 乒乓球碰球拍后反弹",
      "C. 捏橡皮泥变形",
      "D. 拉弹簧变长"
    ],
    "answer": "B",
    "explanation": "反弹是运动方向改变"
  },
  {
    "id": 66,
    "type": "fill",
    "question": "推桌子桌子不动，物体的运动状态___改变。",
    "options": [],
    "answer": "没有",
    "explanation": "推力与摩擦力平衡运动状态没变"
  },
  {
    "id": 67,
    "type": "choice",
    "question": "悬挂的小球受到拉力的施力物体是",
    "options": [
      "A. 地球",
      "B. 小球",
      "C. 细线",
      "D. 手"
    ],
    "answer": "C",
    "explanation": "细线拉住小球，施力物体是细线"
  },
  {
    "id": 68,
    "type": "fill",
    "question": "力用符号___表示。",
    "options": [],
    "answer": "F",
    "explanation": "力用符号F表示"
  },
  {
    "id": 69,
    "type": "choice",
    "question": "磁铁吸引铁钉正确的是",
    "options": [
      "A. 只有磁铁吸引铁钉",
      "B. 磁铁和铁钉互相吸引",
      "C. 只有磁铁是施力物体",
      "D. 只有铁钉是受力物体"
    ],
    "answer": "B",
    "explanation": "力的作用是相互的"
  },
  {
    "id": 70,
    "type": "fill",
    "question": "在力的示意图中，用线段的___表示力的方向。",
    "options": [],
    "answer": "箭头",
    "explanation": "箭头表示力的方向"
  },
  {
    "id": 71,
    "type": "choice",
    "question": "关于重力正确的是",
    "options": [
      "A. 重力是物体本身固有的",
      "B. 重力方向总是垂直向下的",
      "C. 重力是地球对物体的吸引力",
      "D. 空中运动的物体不受重力"
    ],
    "answer": "C",
    "explanation": "重力是由于地球吸引产生的力"
  },
  {
    "id": 72,
    "type": "fill",
    "question": "重力的大小与质量成___比。",
    "options": [],
    "answer": "正",
    "explanation": "G=mg，重力与质量成正比"
  },
  {
    "id": 73,
    "type": "choice",
    "question": "重力的计算公式是",
    "options": [
      "A. G=mg",
      "B. G=m/g",
      "C. G=m²g",
      "D. G=g/m"
    ],
    "answer": "A",
    "explanation": "重力G=mg"
  },
  {
    "id": 74,
    "type": "fill",
    "question": "g=9.8N/kg的物理意义：质量为___kg的物体受到的重力是9.8N。",
    "options": [],
    "answer": "1",
    "explanation": "g表示1kg物体受重力9.8N"
  },
  {
    "id": 75,
    "type": "choice",
    "question": "质量5kg的物体受到的重力是",
    "options": [
      "A. 5N",
      "B. 49N",
      "C. 0.5N",
      "D. 490N"
    ],
    "answer": "B",
    "explanation": "G=5×9.8=49N"
  },
  {
    "id": 76,
    "type": "fill",
    "question": "重力的方向是___向下的。",
    "options": [],
    "answer": "竖直",
    "explanation": "重力方向竖直向下"
  },
  {
    "id": 77,
    "type": "choice",
    "question": "重力的施力物体是",
    "options": [
      "A. 物体本身",
      "B. 地球",
      "C. 支持面",
      "D. 空气"
    ],
    "answer": "B",
    "explanation": "重力的施力物体是地球"
  },
  {
    "id": 78,
    "type": "fill",
    "question": "物体的重力作用点叫做___。",
    "options": [],
    "answer": "重心",
    "explanation": "重力的等效作用点叫重心"
  },
  {
    "id": 79,
    "type": "choice",
    "question": "关于重心正确的是",
    "options": [
      "A. 重心一定在物体上",
      "B. 重心是物体最重的点",
      "C. 形状规则的均匀物体重心在几何中心",
      "D. 重心是受重力最大的点"
    ],
    "answer": "C",
    "explanation": "规则均匀物体重心在几何中心"
  },
  {
    "id": 80,
    "type": "fill",
    "question": "质量2kg的物体受到的重力为___N。（g取10N/kg）",
    "options": [],
    "answer": "20",
    "explanation": "G=2×10=20N"
  },
  {
    "id": 81,
    "type": "choice",
    "question": "宇航员在太空中处于失重状态正确的是",
    "options": [
      "A. 质量变小了",
      "B. 不受重力",
      "C. 仍受重力但表现为失重",
      "D. 没有惯性"
    ],
    "answer": "C",
    "explanation": "太空中仍受地球引力但处于失重状态"
  },
  {
    "id": 82,
    "type": "fill",
    "question": "重垂线利用了重力的方向总是___的原理。",
    "options": [],
    "answer": "竖直向下",
    "explanation": "重垂线利用重力方向竖直向下的特性"
  },
  {
    "id": 83,
    "type": "choice",
    "question": "物体重50N，质量约为",
    "options": [
      "A. 5kg",
      "B. 50kg",
      "C. 500kg",
      "D. 0.5kg"
    ],
    "answer": "A",
    "explanation": "m=50/9.8≈5kg"
  },
  {
    "id": 84,
    "type": "fill",
    "question": "物体在月球上的重力是地球重力的___分之一。",
    "options": [],
    "answer": "六",
    "explanation": "月球上重力约为地球的1/6"
  },
  {
    "id": 85,
    "type": "choice",
    "question": "关于重力和质量正确的是",
    "options": [
      "A. 重力就是质量",
      "B. 重力与质量成正比",
      "C. 质量随重力增大而增大",
      "D. 重力与质量无关"
    ],
    "answer": "B",
    "explanation": "G=mg，重力与质量成正比"
  },
  {
    "id": 86,
    "type": "choice",
    "question": "关于弹力正确的是",
    "options": [
      "A. 只有弹簧才能产生弹力",
      "B. 弹力是物体发生弹性形变时产生的力",
      "C. 弹力就是弹簧的弹力",
      "D. 任何形变都能产生弹力"
    ],
    "answer": "B",
    "explanation": "弹力是弹性形变时产生的力"
  },
  {
    "id": 87,
    "type": "fill",
    "question": "物体发生弹性形变时产生的力叫做___。",
    "options": [],
    "answer": "弹力",
    "explanation": "弹力的定义"
  },
  {
    "id": 88,
    "type": "choice",
    "question": "不属于弹力的是",
    "options": [
      "A. 拉力",
      "B. 压力",
      "C. 支持力",
      "D. 重力"
    ],
    "answer": "D",
    "explanation": "重力不属于弹力"
  },
  {
    "id": 89,
    "type": "fill",
    "question": "弹簧测力计原理：在弹性限度内，伸长量与拉力成___比。",
    "options": [],
    "answer": "正",
    "explanation": "弹簧测力计原理：伸长量与拉力成正比"
  },
  {
    "id": 90,
    "type": "choice",
    "question": "使用弹簧测力计说法错误的是",
    "options": [
      "A. 使用前要调零",
      "B. 使用前观察量程和分度值",
      "C. 测量时可以超过量程",
      "D. 读数时视线与刻度盘垂直"
    ],
    "answer": "C",
    "explanation": "不能超过量程"
  },
  {
    "id": 91,
    "type": "fill",
    "question": "弹簧测力计是测量___的工具。",
    "options": [],
    "answer": "力",
    "explanation": "弹簧测力计测量力的大小"
  },
  {
    "id": 92,
    "type": "choice",
    "question": "弹簧原长10cm挂2N长12cm，挂4N时长",
    "options": [
      "A. 14cm",
      "B. 16cm",
      "C. 18cm",
      "D. 20cm"
    ],
    "answer": "A",
    "explanation": "挂2N伸长2cm，挂4N伸长4cm，总长14cm"
  },
  {
    "id": 93,
    "type": "fill",
    "question": "弹簧测力计的刻度是___的。",
    "options": [],
    "answer": "均匀",
    "explanation": "伸长量与拉力成正比所以刻度均匀"
  },
  {
    "id": 94,
    "type": "choice",
    "question": "关于弹性形变正确的是",
    "options": [
      "A. 形变后都能恢复原状",
      "B. 撤去外力后能恢复原状的形变叫弹性形变",
      "C. 弹性形变就是弹簧的形变",
      "D. 弹性形变不能产生弹力"
    ],
    "answer": "B",
    "explanation": "弹性形变是撤去外力后能恢复原状的形变"
  },
  {
    "id": 95,
    "type": "fill",
    "question": "弹簧测力计内的弹簧受到的力是___力。",
    "options": [],
    "answer": "拉",
    "explanation": "弹簧测力计通过弹簧伸长测量拉力"
  },
  {
    "id": 96,
    "type": "choice",
    "question": "甲乙两弹簧挂2N物体时甲伸长2cm乙伸长3cm则",
    "options": [
      "A. 甲的劲度系数大",
      "B. 乙的劲度系数大",
      "C. 一样大",
      "D. 无法比较"
    ],
    "answer": "A",
    "explanation": "相同拉力下甲伸长短劲度系数更大"
  },
  {
    "id": 97,
    "type": "fill",
    "question": "使用弹簧测力计前应先在___方向上调零。",
    "options": [],
    "answer": "竖直",
    "explanation": "竖直使用应在竖直方向调零"
  },
  {
    "id": 98,
    "type": "choice",
    "question": "弹簧测力计水平放置使用前",
    "options": [
      "A. 不需要调零",
      "B. 应在水平方向调零",
      "C. 只需竖直调零",
      "D. 任何方向调零都行"
    ],
    "answer": "B",
    "explanation": "水平使用应在水平方向调零"
  },
  {
    "id": 99,
    "type": "fill",
    "question": "桌面支持力的本质是___力。",
    "options": [],
    "answer": "弹",
    "explanation": "支持力属于弹力"
  },
  {
    "id": 100,
    "type": "choice",
    "question": "弹簧挂5N长15cm挂3N长13cm则原长为",
    "options": [
      "A. 8cm",
      "B. 9cm",
      "C. 10cm",
      "D. 11cm"
    ],
    "answer": "C",
    "explanation": "5/3=(15-L0)/(13-L0)，解得L0=10cm"
  },
  {
    "id": 101,
    "type": "choice",
    "question": "关于摩擦力正确的是",
    "options": [
      "A. 摩擦力总是阻碍物体运动的",
      "B. 摩擦力总是有害的",
      "C. 摩擦力的方向可以与运动方向相同",
      "D. 不接触也能产生摩擦力"
    ],
    "answer": "C",
    "explanation": "摩擦力有时是动力方向与运动方向相同"
  },
  {
    "id": 102,
    "type": "fill",
    "question": "阻碍相对滑动的力叫做___力。",
    "options": [],
    "answer": "滑动摩擦",
    "explanation": "滑动摩擦力的定义"
  },
  {
    "id": 103,
    "type": "choice",
    "question": "滑动摩擦力与哪个因素有关",
    "options": [
      "A. 运动速度",
      "B. 接触面积",
      "C. 压力大小和接触面粗糙程度",
      "D. 物体体积"
    ],
    "answer": "C",
    "explanation": "滑动摩擦力与压力和接触面粗糙程度有关"
  },
  {
    "id": 104,
    "type": "fill",
    "question": "压力一定时接触面越___滑动摩擦力越大。",
    "options": [],
    "answer": "粗糙",
    "explanation": "压力一定时接触面越粗糙摩擦力越大"
  },
  {
    "id": 105,
    "type": "choice",
    "question": "属于增大摩擦的是",
    "options": [
      "A. 自行车加润滑油",
      "B. 鞋底刻有花纹",
      "C. 气垫船",
      "D. 磁悬浮列车"
    ],
    "answer": "B",
    "explanation": "鞋底花纹增大了接触面粗糙程度"
  },
  {
    "id": 106,
    "type": "fill",
    "question": "接触面粗糙程度一定时压力越___滑动摩擦力越大。",
    "options": [],
    "answer": "大",
    "explanation": "压力越大摩擦力越大"
  },
  {
    "id": 107,
    "type": "choice",
    "question": "属于减小摩擦的是",
    "options": [
      "A. 刹车时用力捏闸",
      "B. 运动鞋底有花纹",
      "C. 给机器加润滑油",
      "D. 拔河时用力握绳"
    ],
    "answer": "C",
    "explanation": "加润滑油减小了摩擦"
  },
  {
    "id": 108,
    "type": "fill",
    "question": "用10N的力推物体匀速运动，摩擦力为___N。",
    "options": [],
    "answer": "10",
    "explanation": "匀速运动时推力与摩擦力平衡"
  },
  {
    "id": 109,
    "type": "choice",
    "question": "关于静摩擦力正确的是",
    "options": [
      "A. 静止的物体一定不受摩擦力",
      "B. 静摩擦力方向与运动趋势方向相反",
      "C. 静摩擦力随推力增大而无限增大",
      "D. 静摩擦力对运动总是有害的"
    ],
    "answer": "B",
    "explanation": "静摩擦力方向与相对运动趋势方向相反"
  },
  {
    "id": 110,
    "type": "fill",
    "question": "人走路时地面对鞋底的摩擦力方向是向___的。",
    "options": [],
    "answer": "前",
    "explanation": "地面对脚的静摩擦力向前是人前进的动力"
  },
  {
    "id": 111,
    "type": "choice",
    "question": "利用了摩擦力的是",
    "options": [
      "A. 滚动轴承",
      "B. 传送带运送货物",
      "C. 冰壶运动",
      "D. 磁悬浮列车"
    ],
    "answer": "B",
    "explanation": "传送带靠摩擦力运送货物"
  },
  {
    "id": 112,
    "type": "fill",
    "question": "用15N水平力推重50N物体匀速运动，摩擦力为___N。",
    "options": [],
    "answer": "15",
    "explanation": "匀速运动时摩擦力=推力=15N"
  },
  {
    "id": 113,
    "type": "choice",
    "question": "木块速度增大为2倍但仍匀速，弹簧测力计示数为",
    "options": [
      "A. 1.5N",
      "B. 3N",
      "C. 6N",
      "D. 9N"
    ],
    "answer": "B",
    "explanation": "滑动摩擦力与速度无关仍为3N"
  },
  {
    "id": 114,
    "type": "fill",
    "question": "用滚动代替滑动可以___摩擦力。",
    "options": [],
    "answer": "减小",
    "explanation": "滚动摩擦远小于滑动摩擦"
  },
  {
    "id": 115,
    "type": "choice",
    "question": "关于摩擦力说法错误的是",
    "options": [
      "A. 摩擦力不一定是阻力",
      "B. 没有摩擦力人就无法行走",
      "C. 摩擦力大小与运动速度有关",
      "D. 摩擦力有时是有益的"
    ],
    "answer": "C",
    "explanation": "滑动摩擦力与运动速度无关"
  },
  {
    "id": 116,
    "type": "fill",
    "question": "汽车轮胎花纹增大了接触面的___程度来增大摩擦。",
    "options": [],
    "answer": "粗糙",
    "explanation": "轮胎花纹增大粗糙程度增大摩擦力"
  },
  {
    "id": 117,
    "type": "choice",
    "question": "木块重10N推力3N匀速，放5N砝码后仍匀速所需推力",
    "options": [
      "A. 3N",
      "B. 4.5N",
      "C. 5N",
      "D. 8N"
    ],
    "answer": "B",
    "explanation": "压力增加1.5倍摩擦力也变为4.5N"
  },
  {
    "id": 118,
    "type": "fill",
    "question": "滑动摩擦力方向与物体___运动方向相反。",
    "options": [],
    "answer": "相对",
    "explanation": "摩擦力方向与相对运动方向相反"
  },
  {
    "id": 119,
    "type": "choice",
    "question": "属于利用摩擦的是",
    "options": [
      "A. 给门轴加润滑油",
      "B. 冰刀很锋利",
      "C. 汽车刹车时踩刹车",
      "D. 拉链上涂蜡"
    ],
    "answer": "C",
    "explanation": "刹车需要摩擦力来减速"
  },
  {
    "id": 120,
    "type": "fill",
    "question": "用6N水平力推木块没推动，摩擦力为___N。",
    "options": [],
    "answer": "6",
    "explanation": "没推动说明推力与静摩擦力平衡"
  },
  {
    "id": 121,
    "type": "choice",
    "question": "牛顿第一定律是",
    "options": [
      "A. 由实验直接得出的",
      "B. 通过实验加推理得出的",
      "C. 由牛顿凭空想象的",
      "D. 由日常经验得出的"
    ],
    "answer": "B",
    "explanation": "牛顿第一定律是在实验基础上通过推理得出的"
  },
  {
    "id": 122,
    "type": "fill",
    "question": "牛顿第一定律：一切物体在没有受力时总保持___状态或匀速直线运动状态。",
    "options": [],
    "answer": "静止",
    "explanation": "牛顿第一定律的内容"
  },
  {
    "id": 123,
    "type": "choice",
    "question": "关于惯性正确的是",
    "options": [
      "A. 只有运动的物体才有惯性",
      "B. 受力时没有惯性",
      "C. 一切物体都有惯性",
      "D. 速度越快惯性越大"
    ],
    "answer": "C",
    "explanation": "惯性是物体固有属性一切物体都有"
  },
  {
    "id": 124,
    "type": "fill",
    "question": "物体保持运动状态不变的性质叫做___。",
    "options": [],
    "answer": "惯性",
    "explanation": "惯性的定义"
  },
  {
    "id": 125,
    "type": "choice",
    "question": "关于惯性说法错误的是",
    "options": [
      "A. 惯性与是否运动无关",
      "B. 惯性与是否受力无关",
      "C. 惯性大小只与质量有关",
      "D. 速度越快惯性越大"
    ],
    "answer": "D",
    "explanation": "惯性大小只与质量有关与速度无关"
  },
  {
    "id": 126,
    "type": "fill",
    "question": "质量越大的物体惯性越___。",
    "options": [],
    "answer": "大",
    "explanation": "质量越大惯性越大"
  },
  {
    "id": 127,
    "type": "choice",
    "question": "汽车突然刹车时乘客向前倾倒是因力",
    "options": [
      "A. 乘客受到向前的力",
      "B. 乘客有惯性要保持原来运动状态",
      "C. 汽车对乘客有向前的推力",
      "D. 乘客受到重力"
    ],
    "answer": "B",
    "explanation": "乘客由于惯性保持原来运动状态"
  },
  {
    "id": 128,
    "type": "fill",
    "question": "锤头松了把锤柄撞击地面利用了锤头的___。",
    "options": [],
    "answer": "惯性",
    "explanation": "锤柄停止运动锤头由于惯性继续向下"
  },
  {
    "id": 129,
    "type": "choice",
    "question": "不是利用惯性的是",
    "options": [
      "A. 跳远时的助跑",
      "B. 拍打衣服除尘",
      "C. 汽车限速行驶",
      "D. 掷出的标枪继续飞行"
    ],
    "answer": "C",
    "explanation": "限速是防止惯性带来的危害"
  },
  {
    "id": 130,
    "type": "fill",
    "question": "牛顿第一定律又叫___定律。",
    "options": [],
    "answer": "惯性",
    "explanation": "牛顿第一定律又叫惯性定律"
  },
  {
    "id": 131,
    "type": "choice",
    "question": "撤去推力后小车慢慢停下来说明",
    "options": [
      "A. 力是维持运动的原因",
      "B. 力是改变运动状态的原因",
      "C. 小车没有惯性",
      "D. 运动不需要力维持"
    ],
    "answer": "B",
    "explanation": "力是改变运动状态的原因"
  },
  {
    "id": 132,
    "type": "fill",
    "question": "一切物体都有保持原来运动状态不变的性质叫做___。",
    "options": [],
    "answer": "惯性",
    "explanation": "惯性的另一种表述"
  },
  {
    "id": 133,
    "type": "choice",
    "question": "属于防止惯性危害的是",
    "options": [
      "A. 掷出的铅球继续飞行",
      "B. 跳远运动员助跑",
      "C. 汽车驾驶员系安全带",
      "D. 拍打被子除尘"
    ],
    "answer": "C",
    "explanation": "系安全带防止紧急刹车时人前冲造成伤害"
  },
  {
    "id": 134,
    "type": "fill",
    "question": "水平面越光滑小车运动得越___。",
    "options": [],
    "answer": "远",
    "explanation": "阻力越小速度减小越慢运动越远"
  },
  {
    "id": 135,
    "type": "choice",
    "question": "表面绝对光滑小车将",
    "options": [
      "A. 立刻停下来",
      "B. 逐渐减速停下",
      "C. 做匀速直线运动",
      "D. 做加速运动"
    ],
    "answer": "C",
    "explanation": "没有阻力小车将做匀速直线运动"
  },
  {
    "id": 136,
    "type": "fill",
    "question": "力不是___物体运动的原因而是改变运动状态的原因。",
    "options": [],
    "answer": "维持",
    "explanation": "力不是维持运动的原因"
  },
  {
    "id": 137,
    "type": "choice",
    "question": "关于力和运动正确的是",
    "options": [
      "A. 力是产生运动的原因",
      "B. 力是维持运动的原因",
      "C. 力是改变运动状态的原因",
      "D. 不受力就不会运动"
    ],
    "answer": "C",
    "explanation": "力是改变运动状态的原因"
  },
  {
    "id": 138,
    "type": "fill",
    "question": "汽车突然向左转弯时车内乘客会向___倾斜。",
    "options": [],
    "answer": "右",
    "explanation": "乘客由于惯性保持原来直线运动状态"
  },
  {
    "id": 139,
    "type": "choice",
    "question": "子弹离枪口后能继续飞行是因为",
    "options": [
      "A. 受到推力",
      "B. 有惯性",
      "C. 受到重力",
      "D. 受到空气阻力"
    ],
    "answer": "B",
    "explanation": "子弹有惯性保持原来运动状态"
  },
  {
    "id": 140,
    "type": "fill",
    "question": "歼击机抛掉副油箱是为了减小飞机的___使运动状态更容易改变。",
    "options": [],
    "answer": "质量",
    "explanation": "减小质量可以减小惯性使飞机更灵活"
  },
  {
    "id": 141,
    "type": "choice",
    "question": "关于平衡力正确的是",
    "options": [
      "A. 两个力大小相等就是平衡力",
      "B. 平衡力作用下一定做匀速直线运动",
      "C. 平衡力作用下保持静止或匀速直线运动状态",
      "D. 平衡力就是重力"
    ],
    "answer": "C",
    "explanation": "平衡力作用下物体处于平衡状态"
  },
  {
    "id": 142,
    "type": "fill",
    "question": "二力平衡条件：作用在___物体上的两个力大小相等方向相反在同一直线上。",
    "options": [],
    "answer": "同一",
    "explanation": "二力平衡条件之一是同一物体"
  },
  {
    "id": 143,
    "type": "choice",
    "question": "哪组力是平衡力",
    "options": [
      "A. 地面对人的支持力和人对地面的压力",
      "B. 两人各用5N的力拉弹簧测力计两端",
      "C. 书对桌面的压力和桌面对书的支持力",
      "D. 竖直上抛小球的重力和空气阻力"
    ],
    "answer": "B",
    "explanation": "两个力作用在同一物体上大小相等方向相反"
  },
  {
    "id": 144,
    "type": "fill",
    "question": "书放在水平桌面上静止，重力和支持力是一对___力。",
    "options": [],
    "answer": "平衡",
    "explanation": "重力和支持力是平衡力"
  },
  {
    "id": 145,
    "type": "choice",
    "question": "书对桌面的压力和桌面对书的支持力是",
    "options": [
      "A. 平衡力",
      "B. 相互作用力",
      "C. 同一个力",
      "D. 以上都不对"
    ],
    "answer": "B",
    "explanation": "分别作用在不同物体上是相互作用力"
  },
  {
    "id": 146,
    "type": "fill",
    "question": "平衡力作用在___物体上，相互作用力作用在___物体上。",
    "options": [],
    "answer": "同一；两个",
    "explanation": "平衡力同体，相互作用力异体"
  },
  {
    "id": 147,
    "type": "choice",
    "question": "重100N的物体用20N水平力推匀速运动，摩擦力为",
    "options": [
      "A. 100N",
      "B. 20N",
      "C. 80N",
      "D. 120N"
    ],
    "answer": "B",
    "explanation": "匀速运动时摩擦力=推力=20N"
  },
  {
    "id": 148,
    "type": "fill",
    "question": "两个力三要素完全相同则___平衡力。",
    "options": [],
    "answer": "不是",
    "explanation": "平衡力方向必须相反"
  },
  {
    "id": 149,
    "type": "choice",
    "question": "钢丝绳吊着500N货物静止，拉力为",
    "options": [
      "A. 大于500N",
      "B. 小于500N",
      "C. 等于500N",
      "D. 无法判断"
    ],
    "answer": "C",
    "explanation": "静止时拉力=重力=500N"
  },
  {
    "id": 150,
    "type": "fill",
    "question": "货物以1m/s匀速上升，拉力为___N。",
    "options": [],
    "answer": "500",
    "explanation": "匀速上升也是平衡状态拉力=重力"
  },
  {
    "id": 151,
    "type": "choice",
    "question": "受到平衡力的是",
    "options": [
      "A. 正在起跑的运动员",
      "B. 自由下落的石块",
      "C. 匀速直线运动的汽车",
      "D. 正在加速的火车"
    ],
    "answer": "C",
    "explanation": "匀速直线运动是平衡状态"
  },
  {
    "id": 152,
    "type": "fill",
    "question": "二力平衡条件中同体是指两个力作用在___物体上。",
    "options": [],
    "answer": "同一",
    "explanation": "同体即同一物体"
  },
  {
    "id": 153,
    "type": "choice",
    "question": "电梯匀速上升时重力和支持力关系是",
    "options": [
      "A. 支持力大于重力",
      "B. 支持力小于重力",
      "C. 支持力等于重力",
      "D. 无法判断"
    ],
    "answer": "C",
    "explanation": "匀速运动是平衡状态支持力等于重力"
  },
  {
    "id": 154,
    "type": "fill",
    "question": "二力平衡条件简记为：同体、等大、___、共线。",
    "options": [],
    "answer": "反向",
    "explanation": "条件为同体等大反向共线"
  },
  {
    "id": 155,
    "type": "choice",
    "question": "人随电梯匀速下降正确的是",
    "options": [
      "A. 支持力大于重力",
      "B. 支持力小于重力",
      "C. 支持力等于重力",
      "D. 只受重力"
    ],
    "answer": "C",
    "explanation": "匀速下降是平衡状态支持力等于重力"
  },
  {
    "id": 156,
    "type": "choice",
    "question": "关于压力正确的是",
    "options": [
      "A. 压力就是重力",
      "B. 压力方向总是竖直向下的",
      "C. 压力是垂直压在物体表面上的力",
      "D. 压力大小一定等于重力"
    ],
    "answer": "C",
    "explanation": "压力是垂直压在物体表面上的力"
  },
  {
    "id": 157,
    "type": "fill",
    "question": "___与受力面积之比叫做压强。",
    "options": [],
    "answer": "压力",
    "explanation": "压强的定义"
  },
  {
    "id": 158,
    "type": "choice",
    "question": "压强的计算公式是",
    "options": [
      "A. p=FS",
      "B. p=F/S",
      "C. p=S/F",
      "D. p=F²S"
    ],
    "answer": "B",
    "explanation": "压强p=F/S"
  },
  {
    "id": 159,
    "type": "fill",
    "question": "压强的单位是___，简称帕。",
    "options": [],
    "answer": "帕斯卡",
    "explanation": "1Pa=1N/m²"
  },
  {
    "id": 160,
    "type": "choice",
    "question": "物体重50N接触面积0.1m²，压强为",
    "options": [
      "A. 5Pa",
      "B. 50Pa",
      "C. 500Pa",
      "D. 5000Pa"
    ],
    "answer": "C",
    "explanation": "p=50/0.1=500Pa"
  },
  {
    "id": 161,
    "type": "fill",
    "question": "增大压强的方法：增大压力或___受力面积。",
    "options": [],
    "answer": "减小",
    "explanation": "增大压力或减小受力面积可增大压强"
  },
  {
    "id": 162,
    "type": "choice",
    "question": "属于增大压强的是",
    "options": [
      "A. 铁轨铺在枕木上",
      "B. 书包背带较宽",
      "C. 刀刃磨得很薄",
      "D. 平板车很多轮子"
    ],
    "answer": "C",
    "explanation": "刀刃磨薄减小受力面积增大压强"
  },
  {
    "id": 163,
    "type": "fill",
    "question": "骆驼蹄子很大是通过增大___面积来减小压强的。",
    "options": [],
    "answer": "受力",
    "explanation": "增大受力面积减小压强"
  },
  {
    "id": 164,
    "type": "choice",
    "question": "关于液体压强正确的是",
    "options": [
      "A. 液体只有向下才产生压强",
      "B. 液体向各个方向都有压强",
      "C. 液体压强只与密度有关",
      "D. 液体压强与深度无关"
    ],
    "answer": "B",
    "explanation": "液体内部向各个方向都有压强"
  },
  {
    "id": 165,
    "type": "fill",
    "question": "液体压强计算公式是p=___。",
    "options": [],
    "answer": "ρgh",
    "explanation": "p=ρgh"
  },
  {
    "id": 166,
    "type": "choice",
    "question": "关于液体压强正确的是",
    "options": [
      "A. 液体压强随深度增大而增大",
      "B. 液体压强随深度增大而减小",
      "C. 液体压强与深度无关",
      "D. 液体压强只与质量有关"
    ],
    "answer": "A",
    "explanation": "同种液体中压强随深度增大而增大"
  },
  {
    "id": 167,
    "type": "fill",
    "question": "液体内同一深度处向各方向压强___。",
    "options": [],
    "answer": "相等",
    "explanation": "同一深度各方向压强相等"
  },
  {
    "id": 168,
    "type": "choice",
    "question": "水深0.5m容器底部压强为（g取10N/kg）",
    "options": [
      "A. 500Pa",
      "B. 5000Pa",
      "C. 50000Pa",
      "D. 50Pa"
    ],
    "answer": "B",
    "explanation": "p=1.0×10³×10×0.5=5000Pa"
  },
  {
    "id": 169,
    "type": "fill",
    "question": "连通器中同种液体不流动时各容器液面___。",
    "options": [],
    "answer": "相平",
    "explanation": "连通器原理"
  },
  {
    "id": 170,
    "type": "choice",
    "question": "不是利用连通器原理的是",
    "options": [
      "A. 茶壶",
      "B. 锅炉水位计",
      "C. 船闸",
      "D. 液体压强计"
    ],
    "answer": "D",
    "explanation": "液体压强计不是连通器"
  },
  {
    "id": 171,
    "type": "fill",
    "question": "马德堡半球实验证明了___的存在。",
    "options": [],
    "answer": "大气压",
    "explanation": "马德堡半球实验证明了大气压存在"
  },
  {
    "id": 172,
    "type": "choice",
    "question": "最早精确测量大气压的实验是",
    "options": [
      "A. 马德堡半球实验",
      "B. 托里拆利实验",
      "C. 帕斯卡裂桶实验",
      "D. 阿基米德实验"
    ],
    "answer": "B",
    "explanation": "托里拆利实验首次精确测量大气压"
  },
  {
    "id": 173,
    "type": "fill",
    "question": "1个标准大气压约为___Pa。",
    "options": [],
    "answer": "1.013×10⁵",
    "explanation": "1标准大气压≈1.013×10⁵Pa"
  },
  {
    "id": 174,
    "type": "choice",
    "question": "关于大气压正确的是",
    "options": [
      "A. 大气压随高度增加而增大",
      "B. 大气压随高度增加而减小",
      "C. 大气压与高度无关",
      "D. 高山上大气压比平地大"
    ],
    "answer": "B",
    "explanation": "海拔越高大气压越小"
  },
  {
    "id": 175,
    "type": "fill",
    "question": "用吸管喝饮料是___的作用使饮料上升到嘴里的。",
    "options": [],
    "answer": "大气压",
    "explanation": "大气压将饮料压入嘴中"
  },
  {
    "id": 176,
    "type": "choice",
    "question": "关于浮力正确的是",
    "options": [
      "A. 只有浮在液面上的物体才受浮力",
      "B. 浸在液体中的物体都受浮力",
      "C. 沉入水底的物体不受浮力",
      "D. 浮力方向向下"
    ],
    "answer": "B",
    "explanation": "浸在液体中的物体都受浮力方向竖直向上"
  },
  {
    "id": 177,
    "type": "fill",
    "question": "浮力的方向是___的。",
    "options": [],
    "answer": "竖直向上",
    "explanation": "浮力方向竖直向上"
  },
  {
    "id": 178,
    "type": "choice",
    "question": "阿基米德原理的内容是",
    "options": [
      "A. 浮力等于物体重力",
      "B. 浮力等于排开液体的重力",
      "C. 浮力等于物体排开液体所受的重力",
      "D. 浮力与深度有关"
    ],
    "answer": "C",
    "explanation": "浮力大小等于排开液体所受的重力"
  },
  {
    "id": 179,
    "type": "fill",
    "question": "阿基米德原理公式：F浮=___。",
    "options": [],
    "answer": "ρ液gV排",
    "explanation": "F浮=ρ液gV排"
  },
  {
    "id": 180,
    "type": "choice",
    "question": "体积100cm³物体完全浸没在水中浮力为（g取10N/kg）",
    "options": [
      "A. 0.1N",
      "B. 1N",
      "C. 10N",
      "D. 100N"
    ],
    "answer": "B",
    "explanation": "F浮=1.0×10³×10×100×10⁻⁶=1N"
  },
  {
    "id": 181,
    "type": "fill",
    "question": "物体漂浮时浮力___重力。",
    "options": [],
    "answer": "=",
    "explanation": "漂浮是平衡状态浮力等于重力"
  },
  {
    "id": 182,
    "type": "choice",
    "question": "轮船从河里驶入海里浮力",
    "options": [
      "A. 增大",
      "B. 减小",
      "C. 不变",
      "D. 无法判断"
    ],
    "answer": "C",
    "explanation": "轮船始终漂浮浮力等于重力不变"
  },
  {
    "id": 183,
    "type": "fill",
    "question": "当F浮___G时物体上浮。",
    "options": [],
    "answer": ">",
    "explanation": "F浮>G时合力向上物体上浮"
  },
  {
    "id": 184,
    "type": "choice",
    "question": "物体放入水中下沉则物体密度与水的密度关系是",
    "options": [
      "A. ρ物>ρ水",
      "B. ρ物<ρ水",
      "C. ρ物=ρ水",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "下沉说明物体密度大于液体密度"
  },
  {
    "id": 185,
    "type": "fill",
    "question": "潜水艇是靠改变___来实现上浮和下沉的。",
    "options": [],
    "answer": "自身重力",
    "explanation": "潜水艇通过充水排水改变自身重力"
  },
  {
    "id": 186,
    "type": "choice",
    "question": "密度计在不同液体中",
    "options": [
      "A. 密度大的液体浸入深",
      "B. 密度小的液体浸入深",
      "C. 不同液体浸入深度相同",
      "D. 无法判断"
    ],
    "answer": "B",
    "explanation": "密度计漂浮浮力不变液体密度小则V排大浸入深"
  },
  {
    "id": 187,
    "type": "fill",
    "question": "热气球靠加热内部空气使其密度___来升空。",
    "options": [],
    "answer": "减小",
    "explanation": "加热空气密度减小气球上升"
  },
  {
    "id": 188,
    "type": "choice",
    "question": "同一物体放入水和酒精中浮力",
    "options": [
      "A. 在水中大",
      "B. 在酒精中大",
      "C. 一样大",
      "D. 无法比较"
    ],
    "answer": "D",
    "explanation": "取决于物体在两种液体中的状态"
  },
  {
    "id": 189,
    "type": "fill",
    "question": "重5N物体放入水中静止后漂浮，浮力为___N。",
    "options": [],
    "answer": "5",
    "explanation": "漂浮时浮力等于重力"
  },
  {
    "id": 190,
    "type": "choice",
    "question": "关于浮力应用说法错误的是",
    "options": [
      "A. 轮船采用空心法增大排水体积",
      "B. 潜水艇靠改变自身重力实现浮沉",
      "C. 气球靠改变自身密度实现升降",
      "D. 密度计在不同液体中浮力不同"
    ],
    "answer": "D",
    "explanation": "密度计在不同液体中都漂浮浮力始终等于自身重力"
  },
  {
    "id": 191,
    "type": "choice",
    "question": "关于杠杆正确的是",
    "options": [
      "A. 杠杆一定是直的",
      "B. 杠杆可以是弯曲的",
      "C. 支点一定在中间",
      "D. 杠杆越长越省力"
    ],
    "answer": "B",
    "explanation": "杠杆可以是弯曲的只要能绕固定点转动"
  },
  {
    "id": 192,
    "type": "fill",
    "question": "杠杆的五要素：支点、动力、阻力、___和阻力臂。",
    "options": [],
    "answer": "动力臂",
    "explanation": "杠杆五要素之一"
  },
  {
    "id": 193,
    "type": "choice",
    "question": "杠杆平衡条件是",
    "options": [
      "A. F1=F2",
      "B. F1×L1=F2×L2",
      "C. F1/L1=F2/L2",
      "D. F1+L1=F2+L2"
    ],
    "answer": "B",
    "explanation": "动力×动力臂=阻力×阻力臂"
  },
  {
    "id": 194,
    "type": "fill",
    "question": "使用杠杆时动力臂越长越___力。",
    "options": [],
    "answer": "省",
    "explanation": "动力臂越长所需动力越小越省力"
  },
  {
    "id": 195,
    "type": "choice",
    "question": "属于省力杠杆的是",
    "options": [
      "A. 钓鱼竿",
      "B. 筷子",
      "C. 撬棒",
      "D. 理发剪刀"
    ],
    "answer": "C",
    "explanation": "撬棒动力臂大于阻力臂是省力杠杆"
  },
  {
    "id": 196,
    "type": "fill",
    "question": "天平是___杠杆。",
    "options": [],
    "answer": "等臂",
    "explanation": "天平动力臂等于阻力臂是等臂杠杆"
  },
  {
    "id": 197,
    "type": "choice",
    "question": "关于定滑轮正确的是",
    "options": [
      "A. 能省力",
      "B. 能省距离",
      "C. 不省力不省距离但能改变力的方向",
      "D. 既省力又能改变力的方向"
    ],
    "answer": "C",
    "explanation": "定滑轮实质是等臂杠杆只能改变力的方向"
  },
  {
    "id": 198,
    "type": "fill",
    "question": "动滑轮的实质是动力臂为阻力臂___倍的杠杆。",
    "options": [],
    "answer": "2",
    "explanation": "动滑轮实质是动力臂为阻力臂2倍的杠杆"
  },
  {
    "id": 199,
    "type": "choice",
    "question": "使用动滑轮提起重物（不计滑轮重和摩擦）拉力为物重的",
    "options": [
      "A. 1倍",
      "B. 1/2",
      "C. 2倍",
      "D. 1/3"
    ],
    "answer": "B",
    "explanation": "动滑轮省一半力拉力=物重/2"
  },
  {
    "id": 200,
    "type": "fill",
    "question": "滑轮组既能___力又能改变力的方向。",
    "options": [],
    "answer": "省",
    "explanation": "滑轮组既能省力又能改变力的方向"
  }
];