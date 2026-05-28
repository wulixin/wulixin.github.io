const ELECTRICITY_QUESTIONS = [
  {
    "id": 1,
    "type": "choice",
    "question": "自然界中存在几种电荷？",
    "options": [
      "A. 1种",
      "B. 2种",
      "C. 3种",
      "D. 4种"
    ],
    "answer": "B",
    "explanation": "自然界只存在两种电荷：正电荷和负电荷。"
  },
  {
    "id": 2,
    "type": "choice",
    "question": "摩擦起电的本质是什么？",
    "options": [
      "A. 创造了电荷",
      "B. 电子的转移",
      "C. 质子的转移",
      "D. 原子的消失"
    ],
    "answer": "B",
    "explanation": "摩擦起电的本质是电子从一个物体转移到另一个物体，并不是创造了电荷。"
  },
  {
    "id": 3,
    "type": "choice",
    "question": "与丝绸摩擦过的玻璃棒带什么电？",
    "options": [
      "A. 正电荷",
      "B. 负电荷",
      "C. 不带电",
      "D. 正负都带"
    ],
    "answer": "A",
    "explanation": "与丝绸摩擦过的玻璃棒失去电子，带正电荷。"
  },
  {
    "id": 4,
    "type": "choice",
    "question": "与毛皮摩擦过的橡胶棒带什么电？",
    "options": [
      "A. 正电荷",
      "B. 负电荷",
      "C. 不带电",
      "D. 正负都带"
    ],
    "answer": "B",
    "explanation": "与毛皮摩擦过的橡胶棒得到电子，带负电荷。"
  },
  {
    "id": 5,
    "type": "choice",
    "question": "两个带同种电荷的物体靠近时会怎样？",
    "options": [
      "A. 相互吸引",
      "B. 相互排斥",
      "C. 无相互作用",
      "D. 可能吸引也可能排斥"
    ],
    "answer": "B",
    "explanation": "同种电荷相互排斥。"
  },
  {
    "id": 6,
    "type": "choice",
    "question": "两个带异种电荷的物体靠近时会怎样？",
    "options": [
      "A. 相互吸引",
      "B. 相互排斥",
      "C. 无相互作用",
      "D. 可能吸引也可能排斥"
    ],
    "answer": "A",
    "explanation": "异种电荷相互吸引。"
  },
  {
    "id": 7,
    "type": "choice",
    "question": "下列关于电流方向的规定，正确的是？",
    "options": [
      "A. 电子移动的方向",
      "B. 正电荷移动的方向",
      "C. 负电荷移动的方向",
      "D. 离子移动的方向"
    ],
    "answer": "B",
    "explanation": "物理学规定正电荷定向移动的方向为电流方向。"
  },
  {
    "id": 8,
    "type": "choice",
    "question": "在金属导体中，实际移动的是什么？",
    "options": [
      "A. 正离子",
      "B. 负离子",
      "C. 自由电子",
      "D. 质子"
    ],
    "answer": "C",
    "explanation": "在金属导体中，自由电子定向移动形成电流。"
  },
  {
    "id": 9,
    "type": "choice",
    "question": "电流的国际单位是什么？",
    "options": [
      "A. 伏特",
      "B. 安培",
      "C. 欧姆",
      "D. 瓦特"
    ],
    "answer": "B",
    "explanation": "电流的国际单位是安培，符号A。"
  },
  {
    "id": 10,
    "type": "choice",
    "question": "验电器的工作原理是什么？",
    "options": [
      "A. 异种电荷相互吸引",
      "B. 同种电荷相互排斥",
      "C. 带电体能吸引轻小物体",
      "D. 摩擦起电"
    ],
    "answer": "B",
    "explanation": "验电器金属箔张开是因为两片箔带同种电荷相互排斥。"
  },
  {
    "id": 11,
    "type": "choice",
    "question": "1A等于多少mA？",
    "options": [
      "A. 10",
      "B. 100",
      "C. 1000",
      "D. 10000"
    ],
    "answer": "C",
    "explanation": "1A=1000mA。"
  },
  {
    "id": 12,
    "type": "choice",
    "question": "下列物体中属于导体的是？",
    "options": [
      "A. 橡胶",
      "B. 塑料",
      "C. 人体",
      "D. 玻璃"
    ],
    "answer": "C",
    "explanation": "人体是导体，橡胶、塑料、玻璃是绝缘体。"
  },
  {
    "id": 13,
    "type": "choice",
    "question": "下列物体中属于绝缘体的是？",
    "options": [
      "A. 铜丝",
      "B. 食盐水",
      "C. 大地",
      "D. 陶瓷"
    ],
    "answer": "D",
    "explanation": "陶瓷是绝缘体，铜丝、食盐水、大地都是导体。"
  },
  {
    "id": 14,
    "type": "fill",
    "question": "电荷间的相互作用规律是：同种电荷相互___，异种电荷相互___。",
    "options": [],
    "answer": "排斥；吸引",
    "explanation": "同种电荷相互排斥，异种电荷相互吸引。"
  },
  {
    "id": 15,
    "type": "fill",
    "question": "摩擦起电并不是创造了电荷，而是电子从一个物体___到另一个物体。",
    "options": [],
    "answer": "转移",
    "explanation": "摩擦起电的本质是电子的转移。"
  },
  {
    "id": 16,
    "type": "fill",
    "question": "丝绸摩擦玻璃棒，玻璃棒失去电子带___电，丝绸得到电子带___电。",
    "options": [],
    "answer": "正；负",
    "explanation": "玻璃棒失去电子带正电，丝绸得到电子带负电。"
  },
  {
    "id": 17,
    "type": "fill",
    "question": "电流是由电荷的___移动形成的。",
    "options": [],
    "answer": "定向",
    "explanation": "电荷的定向移动形成电流。"
  },
  {
    "id": 18,
    "type": "fill",
    "question": "在金属导体中，自由电子定向移动的方向与电流方向___。",
    "options": [],
    "answer": "相反",
    "explanation": "电子带负电，其移动方向与规定的电流方向相反。"
  },
  {
    "id": 19,
    "type": "fill",
    "question": "1mA=___A。",
    "options": [],
    "answer": "0.001",
    "explanation": "1mA=10⁻³A=0.001A。"
  },
  {
    "id": 20,
    "type": "fill",
    "question": "容易导电的物体叫___，不容易导电的物体叫___。",
    "options": [],
    "answer": "导体；绝缘体",
    "explanation": "容易导电的叫导体，不容易导电的叫绝缘体。"
  },
  {
    "id": 21,
    "type": "fill",
    "question": "常见的导体有：金属、人体、大地、___溶液。",
    "options": [],
    "answer": "酸碱盐",
    "explanation": "酸碱盐的水溶液是导体。"
  },
  {
    "id": 22,
    "type": "fill",
    "question": "导体容易导电的原因是导体中有大量的___电荷。",
    "options": [],
    "answer": "自由",
    "explanation": "导体中有大量自由电荷可以定向移动。"
  },
  {
    "id": 23,
    "type": "fill",
    "question": "绝缘体不容易导电的原因是绝缘体中几乎没有___电荷。",
    "options": [],
    "answer": "自由",
    "explanation": "绝缘体中几乎没有自由电荷。"
  },
  {
    "id": 24,
    "type": "fill",
    "question": "验电器带电时两片金属箔张开，是因为同种电荷相互___。",
    "options": [],
    "answer": "排斥",
    "explanation": "同种电荷相互排斥使金属箔张开。"
  },
  {
    "id": 25,
    "type": "fill",
    "question": "一个带电体靠近不带电的轻小物体时，轻小物体会被___。",
    "options": [],
    "answer": "吸引",
    "explanation": "带电体有吸引轻小物体的性质。"
  },
  {
    "id": 26,
    "type": "choice",
    "question": "电路由哪几部分组成？",
    "options": [
      "A. 电源、用电器、开关",
      "B. 电源、用电器、导线",
      "C. 电源、用电器、开关、导线",
      "D. 用电器、开关、导线"
    ],
    "answer": "C",
    "explanation": "电路由电源、用电器、开关和导线四部分组成。"
  },
  {
    "id": 27,
    "type": "choice",
    "question": "电路中提供电能的是？",
    "options": [
      "A. 用电器",
      "B. 开关",
      "C. 导线",
      "D. 电源"
    ],
    "answer": "D",
    "explanation": "电源是提供电能的装置。"
  },
  {
    "id": 28,
    "type": "choice",
    "question": "电路中消耗电能的是？",
    "options": [
      "A. 电源",
      "B. 用电器",
      "C. 开关",
      "D. 导线"
    ],
    "answer": "B",
    "explanation": "用电器是消耗电能的装置。"
  },
  {
    "id": 29,
    "type": "choice",
    "question": "下列电路状态中，会发生危险的是？",
    "options": [
      "A. 通路",
      "B. 断路",
      "C. 短路",
      "D. 开路"
    ],
    "answer": "C",
    "explanation": "短路时电流很大，会烧毁电路甚至引发火灾。"
  },
  {
    "id": 30,
    "type": "choice",
    "question": "串联电路中各处的电流有什么关系？",
    "options": [
      "A. 处处相等",
      "B. 逐渐增大",
      "C. 逐渐减小",
      "D. 不确定"
    ],
    "answer": "A",
    "explanation": "串联电路中各处电流相等。"
  },
  {
    "id": 31,
    "type": "choice",
    "question": "并联电路中干路电流与各支路电流的关系是？",
    "options": [
      "A. 干路电流等于各支路电流",
      "B. 干路电流等于各支路电流之和",
      "C. 干路电流等于各支路电流之积",
      "D. 无确定关系"
    ],
    "answer": "B",
    "explanation": "并联电路中干路电流等于各支路电流之和。"
  },
  {
    "id": 32,
    "type": "choice",
    "question": "串联电路的总电阻与各分电阻的关系是？",
    "options": [
      "A. 总电阻等于各分电阻之积",
      "B. 总电阻等于各分电阻之和",
      "C. 总电阻小于任一分电阻",
      "D. 总电阻等于各分电阻的倒数之和"
    ],
    "answer": "B",
    "explanation": "串联电路总电阻等于各分电阻之和。"
  },
  {
    "id": 33,
    "type": "choice",
    "question": "并联电路的总电阻与各分电阻的关系是？",
    "options": [
      "A. 总电阻等于各分电阻之和",
      "B. 总电阻大于任一分电阻",
      "C. 总电阻小于任一分电阻",
      "D. 总电阻等于各分电阻之积"
    ],
    "answer": "C",
    "explanation": "并联电路总电阻小于任一分电阻。"
  },
  {
    "id": 34,
    "type": "choice",
    "question": "串联电路中，电阻两端的电压分配规律是？",
    "options": [
      "A. 电压处处相等",
      "B. 电压与电阻成正比",
      "C. 电压与电阻成反比",
      "D. 无确定关系"
    ],
    "answer": "B",
    "explanation": "串联电路中电压与电阻成正比，即U1/U2=R1/R2。"
  },
  {
    "id": 35,
    "type": "choice",
    "question": "并联电路中，各支路两端的电压有什么关系？",
    "options": [
      "A. 处处相等",
      "B. 与电阻成正比",
      "C. 与电流成正比",
      "D. 不确定"
    ],
    "answer": "A",
    "explanation": "并联电路各支路两端电压相等。"
  },
  {
    "id": 36,
    "type": "choice",
    "question": "家庭电路中，电灯、电视、冰箱之间的连接方式是？",
    "options": [
      "A. 串联",
      "B. 并联",
      "C. 混联",
      "D. 不确定"
    ],
    "answer": "B",
    "explanation": "家庭电路中各用电器并联连接，互不影响。"
  },
  {
    "id": 37,
    "type": "choice",
    "question": "两个电阻串联，总电阻比其中任何一个分电阻都？",
    "options": [
      "A. 小",
      "B. 大",
      "C. 相等",
      "D. 不确定"
    ],
    "answer": "B",
    "explanation": "串联总电阻等于各分电阻之和，故总电阻比任一分电阻都大。"
  },
  {
    "id": 38,
    "type": "choice",
    "question": "两个电阻并联，总电阻比其中任何一个分电阻都？",
    "options": [
      "A. 小",
      "B. 大",
      "C. 相等",
      "D. 不确定"
    ],
    "answer": "A",
    "explanation": "并联总电阻比任一分电阻都小。"
  },
  {
    "id": 39,
    "type": "choice",
    "question": "串联电路中，若一个用电器断路，其他用电器会？",
    "options": [
      "A. 正常工作",
      "B. 不能工作",
      "C. 电流增大",
      "D. 电压升高"
    ],
    "answer": "B",
    "explanation": "串联电路中各用电器相互影响，一个断路则全部不能工作。"
  },
  {
    "id": 40,
    "type": "choice",
    "question": "并联电路中，若一条支路断开，其他支路会？",
    "options": [
      "A. 不能工作",
      "B. 正常工作",
      "C. 电流增大",
      "D. 电压降低"
    ],
    "answer": "B",
    "explanation": "并联电路中各支路互不影响。"
  },
  {
    "id": 41,
    "type": "fill",
    "question": "处处连通的电路叫___。",
    "options": [],
    "answer": "通路",
    "explanation": "处处连通的电路叫通路。"
  },
  {
    "id": 42,
    "type": "fill",
    "question": "某处断开的电路叫___。",
    "options": [],
    "answer": "断路",
    "explanation": "某处断开的电路叫断路，也叫开路。"
  },
  {
    "id": 43,
    "type": "fill",
    "question": "电源两极直接相连的电路叫___。",
    "options": [],
    "answer": "短路",
    "explanation": "电源两极直接相连叫短路，会烧毁电源。"
  },
  {
    "id": 44,
    "type": "fill",
    "question": "串联电路中，电流___（填处处相等或不相等）。",
    "options": [],
    "answer": "处处相等",
    "explanation": "串联电路各处电流相等。"
  },
  {
    "id": 45,
    "type": "fill",
    "question": "串联电路中，总电压等于各部分电路两端电压___。",
    "options": [],
    "answer": "之和",
    "explanation": "串联电路总电压等于各部分电压之和。"
  },
  {
    "id": 46,
    "type": "fill",
    "question": "并联电路中，干路电流等于各___电流之和。",
    "options": [],
    "answer": "支路",
    "explanation": "并联电路干路电流等于各支路电流之和。"
  },
  {
    "id": 47,
    "type": "fill",
    "question": "并联电路中，各支路两端电压___。",
    "options": [],
    "answer": "相等",
    "explanation": "并联电路各支路两端电压相等。"
  },
  {
    "id": 48,
    "type": "fill",
    "question": "两个电阻R1=6Ω，R2=12Ω串联，总电阻为___Ω。",
    "options": [],
    "answer": "18",
    "explanation": "R=R1+R2=6+12=18Ω。"
  },
  {
    "id": 49,
    "type": "fill",
    "question": "两个电阻R1=6Ω，R2=12Ω并联，总电阻为___Ω。",
    "options": [],
    "answer": "4",
    "explanation": "1/R=1/R1+1/R2=1/6+1/12=3/12=1/4，R=4Ω。"
  },
  {
    "id": 50,
    "type": "fill",
    "question": "n个阻值均为R的电阻串联，总电阻为___。",
    "options": [],
    "answer": "nR",
    "explanation": "n个相同电阻串联，总电阻为nR。"
  },
  {
    "id": 51,
    "type": "fill",
    "question": "n个阻值均为R的电阻并联，总电阻为___。",
    "options": [],
    "answer": "R/n",
    "explanation": "n个相同电阻并联，总电阻为R/n。"
  },
  {
    "id": 52,
    "type": "fill",
    "question": "串联电路中各用电器___（填相互影响或互不影响）。",
    "options": [],
    "answer": "相互影响",
    "explanation": "串联电路中各用电器相互影响。"
  },
  {
    "id": 53,
    "type": "fill",
    "question": "并联电路中各用电器___（填相互影响或互不影响）。",
    "options": [],
    "answer": "互不影响",
    "explanation": "并联电路中各用电器互不影响。"
  },
  {
    "id": 54,
    "type": "fill",
    "question": "街道上的路灯是___联的，因为一盏灯坏了其他灯仍能发光。",
    "options": [],
    "answer": "并",
    "explanation": "路灯并联，互不影响。"
  },
  {
    "id": 55,
    "type": "fill",
    "question": "节日装饰用的小彩灯通常是___联的。",
    "options": [],
    "answer": "串",
    "explanation": "装饰用小彩灯通常串联，一个灯坏了全部不亮。"
  },
  {
    "id": 56,
    "type": "choice",
    "question": "电压的国际单位是什么？",
    "options": [
      "A. 安培",
      "B. 伏特",
      "C. 欧姆",
      "D. 瓦特"
    ],
    "answer": "B",
    "explanation": "电压的国际单位是伏特，符号V。"
  },
  {
    "id": 57,
    "type": "choice",
    "question": "电阻的国际单位是什么？",
    "options": [
      "A. 安培",
      "B. 伏特",
      "C. 欧姆",
      "D. 瓦特"
    ],
    "answer": "C",
    "explanation": "电阻的国际单位是欧姆，符号Ω。"
  },
  {
    "id": 58,
    "type": "choice",
    "question": "一节干电池的电压通常是？",
    "options": [
      "A. 1V",
      "B. 1.5V",
      "C. 3V",
      "D. 220V"
    ],
    "answer": "B",
    "explanation": "一节干电池的电压为1.5V。"
  },
  {
    "id": 59,
    "type": "choice",
    "question": "我国家庭电路的电压是？",
    "options": [
      "A. 110V",
      "B. 220V",
      "C. 380V",
      "D. 36V"
    ],
    "answer": "B",
    "explanation": "我国家庭电路电压为220V。"
  },
  {
    "id": 60,
    "type": "choice",
    "question": "对人体安全的电压是？",
    "options": [
      "A. 不高于220V",
      "B. 不高于36V",
      "C. 不高于12V",
      "D. 不高于110V"
    ],
    "answer": "B",
    "explanation": "不高于36V的电压对人体是安全的。"
  },
  {
    "id": 61,
    "type": "choice",
    "question": "下列关于电阻的说法正确的是？",
    "options": [
      "A. 导体中有电流时才有电阻",
      "B. 电阻是导体本身的性质",
      "C. 电压越大电阻越大",
      "D. 电流越大电阻越小"
    ],
    "answer": "B",
    "explanation": "电阻是导体本身的性质，与电流、电压无关。"
  },
  {
    "id": 62,
    "type": "choice",
    "question": "导体的电阻与下列哪个因素无关？",
    "options": [
      "A. 长度",
      "B. 横截面积",
      "C. 材料",
      "D. 两端电压"
    ],
    "answer": "D",
    "explanation": "电阻与导体的长度、横截面积、材料有关，与两端电压无关。"
  },
  {
    "id": 63,
    "type": "choice",
    "question": "同种材料的导体，长度越长，电阻怎样变化？",
    "options": [
      "A. 越大",
      "B. 越小",
      "C. 不变",
      "D. 不确定"
    ],
    "answer": "A",
    "explanation": "同种材料的导体，长度越长，电阻越大。"
  },
  {
    "id": 64,
    "type": "choice",
    "question": "同种材料的导体，横截面积越大，电阻怎样变化？",
    "options": [
      "A. 越大",
      "B. 越小",
      "C. 不变",
      "D. 不确定"
    ],
    "answer": "B",
    "explanation": "同种材料的导体，横截面积越大，电阻越小。"
  },
  {
    "id": 65,
    "type": "choice",
    "question": "1kΩ等于多少Ω？",
    "options": [
      "A. 10",
      "B. 100",
      "C. 1000",
      "D. 10000"
    ],
    "answer": "C",
    "explanation": "1kΩ=1000Ω。"
  },
  {
    "id": 66,
    "type": "choice",
    "question": "1MΩ等于多少Ω？",
    "options": [
      "A. 10³",
      "B. 10⁶",
      "C. 10⁹",
      "D. 10¹²"
    ],
    "answer": "B",
    "explanation": "1MΩ=10⁶Ω。"
  },
  {
    "id": 67,
    "type": "choice",
    "question": "滑动变阻器的工作原理是？",
    "options": [
      "A. 改变导体的材料",
      "B. 改变导体的温度",
      "C. 改变接入电路中电阻丝的长度",
      "D. 改变导体的横截面积"
    ],
    "answer": "C",
    "explanation": "滑动变阻器通过改变接入电路中电阻丝的长度来改变电阻。"
  },
  {
    "id": 68,
    "type": "choice",
    "question": "滑动变阻器铭牌20Ω 1A中，1A表示？",
    "options": [
      "A. 最小电流",
      "B. 最大电流",
      "C. 额定电流",
      "D. 允许通过的最大电流"
    ],
    "answer": "D",
    "explanation": "1A表示滑动变阻器允许通过的最大电流为1A。"
  },
  {
    "id": 69,
    "type": "fill",
    "question": "1kV=___V。",
    "options": [],
    "answer": "1000",
    "explanation": "1kV=1000V。"
  },
  {
    "id": 70,
    "type": "fill",
    "question": "1V=___mV。",
    "options": [],
    "answer": "1000",
    "explanation": "1V=1000mV。"
  },
  {
    "id": 71,
    "type": "fill",
    "question": "一节蓄电池的电压为___V。",
    "options": [],
    "answer": "2",
    "explanation": "一节蓄电池的电压为2V。"
  },
  {
    "id": 72,
    "type": "fill",
    "question": "三节干电池串联后的总电压为___V。",
    "options": [],
    "answer": "4.5",
    "explanation": "3×1.5V=4.5V。"
  },
  {
    "id": 73,
    "type": "fill",
    "question": "电压是使电路中形成___的原因。",
    "options": [],
    "answer": "电流",
    "explanation": "电压是使电路中形成电流的原因。"
  },
  {
    "id": 74,
    "type": "fill",
    "question": "电阻是导体本身的一种___，它的大小与导体两端的电压和通过导体的电流___关。",
    "options": [],
    "answer": "性质；无",
    "explanation": "电阻是导体的固有性质，与电压和电流无关。"
  },
  {
    "id": 75,
    "type": "fill",
    "question": "导体的电阻与导体的___、___和___有关。",
    "options": [],
    "answer": "长度；横截面积；材料",
    "explanation": "导体的电阻与长度、横截面积和材料有关。"
  },
  {
    "id": 76,
    "type": "fill",
    "question": "导体的电阻还与___有关，大多数导体的电阻随温度升高而增大。",
    "options": [],
    "answer": "温度",
    "explanation": "导体的电阻还与温度有关。"
  },
  {
    "id": 77,
    "type": "fill",
    "question": "滑动变阻器是通过改变接入电路中电阻丝的___来改变电阻的。",
    "options": [],
    "answer": "长度",
    "explanation": "滑动变阻器改变电阻丝接入长度来改变电阻。"
  },
  {
    "id": 78,
    "type": "fill",
    "question": "滑动变阻器接入电路时应采用一上一下的接法，如果同时接上面两个接线柱，则接入电路的电阻为___。",
    "options": [],
    "answer": "0",
    "explanation": "同时接上面两个接线柱，相当于导线直接连接，接入电阻为0。"
  },
  {
    "id": 79,
    "type": "fill",
    "question": "在连接滑动变阻器时，闭合开关前应将滑片移到___阻值处。",
    "options": [],
    "answer": "最大",
    "explanation": "闭合开关前应将滑片移到最大阻值处，保护电路。"
  },
  {
    "id": 80,
    "type": "fill",
    "question": "三节蓄电池串联后的总电压为___V。",
    "options": [],
    "answer": "6",
    "explanation": "3×2V=6V。"
  },
  {
    "id": 81,
    "type": "choice",
    "question": "欧姆定律的数学表达式是？",
    "options": [
      "A. I=UR",
      "B. I=U/R",
      "C. I=U+R",
      "D. I=U-R"
    ],
    "answer": "B",
    "explanation": "欧姆定律公式为I=U/R。"
  },
  {
    "id": 82,
    "type": "choice",
    "question": "由欧姆定律I=U/R可得R=U/I，关于这个公式下列说法正确的是？",
    "options": [
      "A. 导体电阻与电压成正比",
      "B. 导体电阻与电流成反比",
      "C. 电阻是导体本身的性质，与电压电流无关",
      "D. 电压为零时电阻为零"
    ],
    "answer": "C",
    "explanation": "R=U/I只是计算式，电阻是导体本身的性质，不随电压和电流改变。"
  },
  {
    "id": 83,
    "type": "choice",
    "question": "一个10Ω的电阻两端加6V电压，通过的电流是？",
    "options": [
      "A. 0.6A",
      "B. 60A",
      "C. 0.06A",
      "D. 1.67A"
    ],
    "answer": "A",
    "explanation": "I=U/R=6V/10Ω=0.6A。"
  },
  {
    "id": 84,
    "type": "choice",
    "question": "一个5Ω的电阻通过0.4A的电流，它两端的电压是？",
    "options": [
      "A. 0.08V",
      "B. 2V",
      "C. 12.5V",
      "D. 1.25V"
    ],
    "answer": "B",
    "explanation": "U=IR=0.4A×5Ω=2V。"
  },
  {
    "id": 85,
    "type": "choice",
    "question": "一个导体两端加4V电压时通过0.2A电流，该导体的电阻是？",
    "options": [
      "A. 0.05Ω",
      "B. 0.8Ω",
      "C. 20Ω",
      "D. 2Ω"
    ],
    "answer": "C",
    "explanation": "R=U/I=4V/0.2A=20Ω。"
  },
  {
    "id": 86,
    "type": "choice",
    "question": "当导体两端电压一定时，通过导体的电流与电阻的关系是？",
    "options": [
      "A. 成正比",
      "B. 成反比",
      "C. 无关",
      "D. 相等"
    ],
    "answer": "B",
    "explanation": "由I=U/R知，电压一定时，电流与电阻成反比。"
  },
  {
    "id": 87,
    "type": "choice",
    "question": "当导体电阻一定时，通过导体的电流与电压的关系是？",
    "options": [
      "A. 成正比",
      "B. 成反比",
      "C. 无关",
      "D. 相等"
    ],
    "answer": "A",
    "explanation": "由I=U/R知，电阻一定时，电流与电压成正比。"
  },
  {
    "id": 88,
    "type": "choice",
    "question": "R1=10Ω和R2=20Ω串联，接在6V电源上，通过R1的电流是？",
    "options": [
      "A. 0.6A",
      "B. 0.3A",
      "C. 0.2A",
      "D. 0.4A"
    ],
    "answer": "C",
    "explanation": "I=U/(R1+R2)=6V/30Ω=0.2A。"
  },
  {
    "id": 89,
    "type": "choice",
    "question": "R1=10Ω和R2=20Ω串联，接在6V电源上，R1两端的电压是？",
    "options": [
      "A. 4V",
      "B. 3V",
      "C. 2V",
      "D. 1V"
    ],
    "answer": "C",
    "explanation": "I=0.2A，U1=IR1=0.2A×10Ω=2V。"
  },
  {
    "id": 90,
    "type": "choice",
    "question": "R1=6Ω和R2=12Ω并联，接在6V电源上，干路总电流是？",
    "options": [
      "A. 1A",
      "B. 1.5A",
      "C. 0.5A",
      "D. 2A"
    ],
    "answer": "B",
    "explanation": "I1=6V/6Ω=1A，I2=6V/12Ω=0.5A，I=1.5A。"
  },
  {
    "id": 91,
    "type": "choice",
    "question": "一个电阻两端电压从4V增大到6V，通过的电流增加了0.2A，该电阻的阻值是？",
    "options": [
      "A. 10Ω",
      "B. 20Ω",
      "C. 30Ω",
      "D. 15Ω"
    ],
    "answer": "A",
    "explanation": "R=ΔU/ΔI=(6-4)V/0.2A=10Ω。"
  },
  {
    "id": 92,
    "type": "choice",
    "question": "某导体两端电压为6V时，通过电流为0.3A，当电压降为0V时，该导体的电阻为？",
    "options": [
      "A. 0Ω",
      "B. 20Ω",
      "C. 10Ω",
      "D. 无穷大"
    ],
    "answer": "B",
    "explanation": "R=U/I=6V/0.3A=20Ω，电阻不随电压改变。"
  },
  {
    "id": 93,
    "type": "choice",
    "question": "两个电阻串联，R1:R2=1:2，则U1:U2等于？",
    "options": [
      "A. 1:2",
      "B. 2:1",
      "C. 1:1",
      "D. 1:4"
    ],
    "answer": "A",
    "explanation": "串联电路中电压与电阻成正比，U1:U2=R1:R2=1:2。"
  },
  {
    "id": 94,
    "type": "choice",
    "question": "两个电阻并联，R1:R2=1:2，则I1:I2等于？",
    "options": [
      "A. 1:2",
      "B. 2:1",
      "C. 1:1",
      "D. 1:4"
    ],
    "answer": "B",
    "explanation": "并联电路中电流与电阻成反比，I1:I2=R2:R1=2:1。"
  },
  {
    "id": 95,
    "type": "choice",
    "question": "欧姆定律适用的条件是？",
    "options": [
      "A. 任何电路",
      "B. 纯电阻电路",
      "C. 含电动机的电路",
      "D. 含电感的电路"
    ],
    "answer": "B",
    "explanation": "欧姆定律适用于纯电阻电路。"
  },
  {
    "id": 96,
    "type": "fill",
    "question": "欧姆定律的内容：导体中的电流与导体两端的电压成___，与导体的电阻成___。",
    "options": [],
    "answer": "正比；反比",
    "explanation": "导体中的电流与电压成正比，与电阻成反比。"
  },
  {
    "id": 97,
    "type": "fill",
    "question": "欧姆定律的公式为___，其中I的单位是A，U的单位是V，R的单位是Ω。",
    "options": [],
    "answer": "I=U/R",
    "explanation": "欧姆定律公式I=U/R。"
  },
  {
    "id": 98,
    "type": "fill",
    "question": "一个20Ω的电阻，两端加10V电压，通过的电流为___A。",
    "options": [],
    "answer": "0.5",
    "explanation": "I=U/R=10V/20Ω=0.5A。"
  },
  {
    "id": 99,
    "type": "fill",
    "question": "一个导体通过0.5A电流时两端电压为6V，若电流增大到1A，两端电压为___V。",
    "options": [],
    "answer": "12",
    "explanation": "R=6V/0.5A=12Ω，U=1A×12Ω=12V。"
  },
  {
    "id": 100,
    "type": "fill",
    "question": "R1=10Ω和R2=20Ω串联，通过R1的电流为0.3A，则通过R2的电流为___A。",
    "options": [],
    "answer": "0.3",
    "explanation": "串联电路各处电流相等，所以I2=I1=0.3A。"
  },
  {
    "id": 101,
    "type": "fill",
    "question": "R1=5Ω和R2=15Ω串联，总电压为8V，则R2两端的电压为___V。",
    "options": [],
    "answer": "6",
    "explanation": "I=8V/20Ω=0.4A，U2=0.4A×15Ω=6V。"
  },
  {
    "id": 102,
    "type": "fill",
    "question": "R1=10Ω和R2=30Ω并联，通过R1的电流为0.6A，则通过R2的电流为___A。",
    "options": [],
    "answer": "0.2",
    "explanation": "U=0.6A×10Ω=6V，I2=6V/30Ω=0.2A。"
  },
  {
    "id": 103,
    "type": "fill",
    "question": "一个电阻两端电压从2V增大到6V，通过它的电流从0.1A增大到___A。",
    "options": [],
    "answer": "0.3",
    "explanation": "R=2V/0.1A=20Ω，I=6V/20Ω=0.3A。"
  },
  {
    "id": 104,
    "type": "fill",
    "question": "在I-U图像中，某导体的图线是一条过原点的___线。",
    "options": [],
    "answer": "直",
    "explanation": "欧姆定律中I与U成正比，I-U图像是过原点的直线。"
  },
  {
    "id": 105,
    "type": "fill",
    "question": "在I-U图像中，图线的斜率表示___的倒数。",
    "options": [],
    "answer": "电阻",
    "explanation": "I-U图像斜率k=I/U=1/R，即电阻的倒数。"
  },
  {
    "id": 106,
    "type": "fill",
    "question": "串联电路中，电压与电阻成___比。",
    "options": [],
    "answer": "正",
    "explanation": "串联电路中电压分配与电阻成正比。"
  },
  {
    "id": 107,
    "type": "fill",
    "question": "并联电路中，电流与电阻成___比。",
    "options": [],
    "answer": "反",
    "explanation": "并联电路中电流分配与电阻成反比。"
  },
  {
    "id": 108,
    "type": "fill",
    "question": "一个定值电阻R=50Ω，通过0.2A电流，两端电压为___V。",
    "options": [],
    "answer": "10",
    "explanation": "U=IR=0.2A×50Ω=10V。"
  },
  {
    "id": 109,
    "type": "fill",
    "question": "R1=3Ω和R2=6Ω并联后的总电阻为___Ω。",
    "options": [],
    "answer": "2",
    "explanation": "1/R=1/3+1/6=3/6=1/2，R=2Ω。"
  },
  {
    "id": 110,
    "type": "fill",
    "question": "R1=4Ω和R2=6Ω串联后的总电阻为___Ω。",
    "options": [],
    "answer": "10",
    "explanation": "R=R1+R2=4+6=10Ω。"
  },
  {
    "id": 111,
    "type": "choice",
    "question": "电功的国际单位是什么？",
    "options": [
      "A. 瓦特",
      "B. 焦耳",
      "C. 伏特",
      "D. 安培"
    ],
    "answer": "B",
    "explanation": "电功的国际单位是焦耳，符号J。"
  },
  {
    "id": 112,
    "type": "choice",
    "question": "电功率的国际单位是什么？",
    "options": [
      "A. 焦耳",
      "B. 伏特",
      "C. 瓦特",
      "D. 安培"
    ],
    "answer": "C",
    "explanation": "电功率的国际单位是瓦特，符号W。"
  },
  {
    "id": 113,
    "type": "choice",
    "question": "电功的公式W=UIt中，W的单位是焦耳，则U、I、t的单位分别是？",
    "options": [
      "A. V、A、s",
      "B. V、A、min",
      "C. kV、A、s",
      "D. V、mA、s"
    ],
    "answer": "A",
    "explanation": "W=UIt，U单位为V，I单位为A，t单位为s时，W单位为J。"
  },
  {
    "id": 114,
    "type": "choice",
    "question": "1度电等于多少焦耳？",
    "options": [
      "A. 3.6×10³J",
      "B. 3.6×10⁶J",
      "C. 3.6×10⁹J",
      "D. 3.6×10¹²J"
    ],
    "answer": "B",
    "explanation": "1度=1kW·h=1000W×3600s=3.6×10⁶J。"
  },
  {
    "id": 115,
    "type": "choice",
    "question": "电功率的公式P=W/t中，P的单位是瓦特，则W、t的单位分别是？",
    "options": [
      "A. J、s",
      "B. J、min",
      "C. kW·h、s",
      "D. J、h"
    ],
    "answer": "A",
    "explanation": "P=W/t，W单位为J，t单位为s时，P单位为W。"
  },
  {
    "id": 116,
    "type": "choice",
    "question": "一个灯泡标有220V 100W，它正常工作时的电流约为？",
    "options": [
      "A. 0.45A",
      "B. 0.22A",
      "C. 2.2A",
      "D. 4.5A"
    ],
    "answer": "A",
    "explanation": "I=P/U=100W/220V≈0.45A。"
  },
  {
    "id": 117,
    "type": "choice",
    "question": "一个灯泡标有220V 100W，它正常工作时的电阻约为？",
    "options": [
      "A. 484Ω",
      "B. 242Ω",
      "C. 968Ω",
      "D. 121Ω"
    ],
    "answer": "A",
    "explanation": "R=U²/P=220²/100=484Ω。"
  },
  {
    "id": 118,
    "type": "choice",
    "question": "额定电压是指？",
    "options": [
      "A. 用电器实际工作的电压",
      "B. 用电器正常工作时的电压",
      "C. 电路中的最大电压",
      "D. 电源电压"
    ],
    "answer": "B",
    "explanation": "额定电压是用电器正常工作时的电压。"
  },
  {
    "id": 119,
    "type": "choice",
    "question": "实际电压大于额定电压时，实际功率与额定功率的关系是？",
    "options": [
      "A. 实际功率等于额定功率",
      "B. 实际功率小于额定功率",
      "C. 实际功率大于额定功率",
      "D. 不确定"
    ],
    "answer": "C",
    "explanation": "实际电压大于额定电压时，实际功率大于额定功率。"
  },
  {
    "id": 120,
    "type": "choice",
    "question": "灯泡的亮度由什么决定？",
    "options": [
      "A. 额定功率",
      "B. 额定电压",
      "C. 实际功率",
      "D. 通过的电流"
    ],
    "answer": "C",
    "explanation": "灯泡的亮度由实际功率决定。"
  },
  {
    "id": 121,
    "type": "choice",
    "question": "标有220V 40W和220V 100W的两盏灯串联在220V电路中，哪盏灯更亮？",
    "options": [
      "A. 40W的灯更亮",
      "B. 100W的灯更亮",
      "C. 一样亮",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "串联电路中，电阻大的分得电压多，实际功率大。40W灯电阻大，实际功率大，更亮。"
  },
  {
    "id": 122,
    "type": "choice",
    "question": "标有220V 40W和220V 100W的两盏灯并联在220V电路中，哪盏灯更亮？",
    "options": [
      "A. 40W的灯更亮",
      "B. 100W的灯更亮",
      "C. 一样亮",
      "D. 无法判断"
    ],
    "answer": "B",
    "explanation": "并联时各灯两端电压等于额定电压，100W灯的实际功率大，更亮。"
  },
  {
    "id": 123,
    "type": "choice",
    "question": "1kW·h的电能可以使100W的灯泡正常发光多少小时？",
    "options": [
      "A. 1h",
      "B. 10h",
      "C. 100h",
      "D. 1000h"
    ],
    "answer": "B",
    "explanation": "t=W/P=1kW·h/0.1kW=10h。"
  },
  {
    "id": 124,
    "type": "choice",
    "question": "下列关于电功率的说法正确的是？",
    "options": [
      "A. 电功率越大，电流做功越多",
      "B. 电功率越大，电流做功越快",
      "C. 电功率越大，消耗电能越多",
      "D. 电功率与电压无关"
    ],
    "answer": "B",
    "explanation": "电功率表示电流做功的快慢，功率越大做功越快。"
  },
  {
    "id": 125,
    "type": "choice",
    "question": "电功率的推导公式P=I²R适用于什么？",
    "options": [
      "A. 任何电路",
      "B. 纯电阻电路",
      "C. 非纯电阻电路",
      "D. 含电动机的电路"
    ],
    "answer": "B",
    "explanation": "P=I²R由欧姆定律推导，仅适用于纯电阻电路。"
  },
  {
    "id": 126,
    "type": "fill",
    "question": "电流做功的过程，实际上是电能转化为___能的过程。",
    "options": [],
    "answer": "其他形式",
    "explanation": "电流做功的过程就是电能转化为其他形式能的过程。"
  },
  {
    "id": 127,
    "type": "fill",
    "question": "电功的公式为W=___。",
    "options": [],
    "answer": "UIt",
    "explanation": "电功公式W=UIt。"
  },
  {
    "id": 128,
    "type": "fill",
    "question": "1度电=___kW·h=___J。",
    "options": [],
    "answer": "1；3.6×10⁶",
    "explanation": "1度=1kW·h=3.6×10⁶J。"
  },
  {
    "id": 129,
    "type": "fill",
    "question": "电功率的物理意义是表示电流做功___的物理量。",
    "options": [],
    "answer": "快慢",
    "explanation": "电功率表示电流做功的快慢。"
  },
  {
    "id": 130,
    "type": "fill",
    "question": "电功率的定义式为P=___。",
    "options": [],
    "answer": "W/t",
    "explanation": "电功率定义式P=W/t。"
  },
  {
    "id": 131,
    "type": "fill",
    "question": "电功率的计算式为P=___和P=___。",
    "options": [],
    "answer": "UI；I²R（或U²/R）",
    "explanation": "电功率计算式P=UI，纯电阻电路还可用P=I²R或P=U²/R。"
  },
  {
    "id": 132,
    "type": "fill",
    "question": "用电器正常工作时的电压叫___电压，此时的功率叫___功率。",
    "options": [],
    "answer": "额定；额定",
    "explanation": "正常工作时的电压叫额定电压，此时的功率叫额定功率。"
  },
  {
    "id": 133,
    "type": "fill",
    "question": "用电器实际工作时的电压叫___电压，此时的功率叫___功率。",
    "options": [],
    "answer": "实际；实际",
    "explanation": "实际工作时的电压叫实际电压，此时的功率叫实际功率。"
  },
  {
    "id": 134,
    "type": "fill",
    "question": "灯泡的亮度由___功率决定。",
    "options": [],
    "answer": "实际",
    "explanation": "灯泡的亮度由实际功率决定，实际功率越大越亮。"
  },
  {
    "id": 135,
    "type": "fill",
    "question": "一个灯泡标有220V 60W，正常工作时通过它的电流约为___A（保留两位小数）。",
    "options": [],
    "answer": "0.27",
    "explanation": "I=P/U=60W/220V≈0.27A。"
  },
  {
    "id": 136,
    "type": "fill",
    "question": "一个电热水器额定功率为2000W，正常工作0.5h消耗的电能为___度。",
    "options": [],
    "answer": "1",
    "explanation": "W=Pt=2kW×0.5h=1kW·h=1度。"
  },
  {
    "id": 137,
    "type": "fill",
    "question": "一个100Ω的电阻通过0.1A电流，10s内电流做的功为___J。",
    "options": [],
    "answer": "10",
    "explanation": "W=I²Rt=0.01×100×10=10J。"
  },
  {
    "id": 138,
    "type": "fill",
    "question": "一个电阻两端电压为12V，通过0.5A电流，电功率为___W。",
    "options": [],
    "answer": "6",
    "explanation": "P=UI=12V×0.5A=6W。"
  },
  {
    "id": 139,
    "type": "fill",
    "question": "1kW=___W。",
    "options": [],
    "answer": "1000",
    "explanation": "1kW=1000W。"
  },
  {
    "id": 140,
    "type": "fill",
    "question": "一个200Ω的电阻两端加10V电压，1min内电流做的功为___J。",
    "options": [],
    "answer": "30",
    "explanation": "P=U²/R=100/200=0.5W，W=0.5×60=30J。"
  },
  {
    "id": 141,
    "type": "choice",
    "question": "焦耳定律的数学表达式是？",
    "options": [
      "A. Q=UIt",
      "B. Q=I²Rt",
      "C. Q=U²/Rt",
      "D. Q=I²R"
    ],
    "answer": "B",
    "explanation": "焦耳定律公式Q=I²Rt。"
  },
  {
    "id": 142,
    "type": "choice",
    "question": "电流产生的热量与下列哪个因素无关？",
    "options": [
      "A. 电流",
      "B. 电阻",
      "C. 通电时间",
      "D. 导体两端的电压"
    ],
    "answer": "D",
    "explanation": "由Q=I²Rt知，热量与电流、电阻、时间有关，与电压无关。"
  },
  {
    "id": 143,
    "type": "choice",
    "question": "两个电阻R1和R2串联，R1>R2，相同时间内哪个产生的热量多？",
    "options": [
      "A. R1产生的多",
      "B. R2产生的多",
      "C. 一样多",
      "D. 无法判断"
    ],
    "answer": "A",
    "explanation": "串联电路电流相同，Q=I²Rt，R大的产生的热量多。"
  },
  {
    "id": 144,
    "type": "choice",
    "question": "两个电阻R1和R2并联，R1>R2，相同时间内哪个产生的热量多？",
    "options": [
      "A. R1产生的多",
      "B. R2产生的多",
      "C. 一样多",
      "D. 无法判断"
    ],
    "answer": "B",
    "explanation": "并联电路电压相同，Q=U²t/R，R小的产生的热量多。"
  },
  {
    "id": 145,
    "type": "choice",
    "question": "电炉丝热得发红，而与它串联的导线几乎不热，原因是？",
    "options": [
      "A. 导线中电流小",
      "B. 导线的电阻比电炉丝小得多",
      "C. 导线的绝缘层散热快",
      "D. 导线的电压高"
    ],
    "answer": "B",
    "explanation": "串联电流相同，Q=I²Rt，导线电阻远小于电炉丝电阻，所以导线产热很少。"
  },
  {
    "id": 146,
    "type": "choice",
    "question": "电流通过导体产生的热量与电流的几次方成正比？",
    "options": [
      "A. 1次方",
      "B. 2次方",
      "C. 3次方",
      "D. 0.5次方"
    ],
    "answer": "B",
    "explanation": "Q=I²Rt，热量与电流的平方（2次方）成正比。"
  },
  {
    "id": 147,
    "type": "choice",
    "question": "一个10Ω的电阻通过2A电流，10s内产生的热量是？",
    "options": [
      "A. 200J",
      "B. 400J",
      "C. 20J",
      "D. 40J"
    ],
    "answer": "B",
    "explanation": "Q=I²Rt=4×10×10=400J。"
  },
  {
    "id": 148,
    "type": "choice",
    "question": "下列电器中，利用电流热效应工作的是？",
    "options": [
      "A. 电风扇",
      "B. 电视机",
      "C. 电饭锅",
      "D. 洗衣机"
    ],
    "answer": "C",
    "explanation": "电饭锅利用电流热效应发热，其余电器主要不是利用热效应。"
  },
  {
    "id": 149,
    "type": "choice",
    "question": "下列电器中，防止电流热效应造成危害的是？",
    "options": [
      "A. 电熨斗",
      "B. 电热毯",
      "C. 电脑散热器",
      "D. 电热水器"
    ],
    "answer": "C",
    "explanation": "电脑散热器用于散热，防止电流热效应造成电脑过热。"
  },
  {
    "id": 150,
    "type": "choice",
    "question": "焦耳定律适用于什么电路？",
    "options": [
      "A. 纯电阻电路",
      "B. 非纯电阻电路",
      "C. 任何电路",
      "D. 只有串联电路"
    ],
    "answer": "C",
    "explanation": "焦耳定律Q=I²Rt适用于任何电路求产生的热量。"
  },
  {
    "id": 151,
    "type": "fill",
    "question": "焦耳定律的内容：电流通过导体产生的热量与电流的___成正比，与导体的电阻成___，与通电时间成___。",
    "options": [],
    "answer": "平方；正比；正比",
    "explanation": "Q=I²Rt，热量与电流平方成正比，与电阻成正比，与时间成正比。"
  },
  {
    "id": 152,
    "type": "fill",
    "question": "焦耳定律的公式为Q=___。",
    "options": [],
    "answer": "I²Rt",
    "explanation": "焦耳定律公式Q=I²Rt。"
  },
  {
    "id": 153,
    "type": "fill",
    "question": "电流通过导体时将电能转化为___能，这种现象叫电流的热效应。",
    "options": [],
    "answer": "内",
    "explanation": "电流的热效应将电能转化为内能。"
  },
  {
    "id": 154,
    "type": "fill",
    "question": "一个20Ω的电阻通过0.5A电流，30s内产生的热量为___J。",
    "options": [],
    "answer": "150",
    "explanation": "Q=I²Rt=0.25×20×30=150J。"
  },
  {
    "id": 155,
    "type": "fill",
    "question": "一个电阻通过1A电流，5s产生100J热量，该电阻为___Ω。",
    "options": [],
    "answer": "20",
    "explanation": "R=Q/(I²t)=100/(1×5)=20Ω。"
  },
  {
    "id": 156,
    "type": "fill",
    "question": "电热器的优点是清洁卫生、没有环境污染、___效率高。",
    "options": [],
    "answer": "热",
    "explanation": "电热器的热效率高。"
  },
  {
    "id": 157,
    "type": "fill",
    "question": "在纯电阻电路中，电流做的功___（填大于、等于或小于）产生的热量。",
    "options": [],
    "answer": "等于",
    "explanation": "纯电阻电路中电能全部转化为内能，W=Q。"
  },
  {
    "id": 158,
    "type": "fill",
    "question": "在非纯电阻电路中（如含电动机），电流做的功___（填大于、等于或小于）产生的热量。",
    "options": [],
    "answer": "大于",
    "explanation": "非纯电阻电路中电能转化为内能和其他形式能，W>Q。"
  },
  {
    "id": 159,
    "type": "fill",
    "question": "一个电热丝电阻为50Ω，通过2A电流，1min产生的热量为___J。",
    "options": [],
    "answer": "12000",
    "explanation": "Q=I²Rt=4×50×60=12000J。"
  },
  {
    "id": 160,
    "type": "fill",
    "question": "两个相同的电阻R，先串联后并联接到同一电源上，串联时总热量与并联时总热量之比为___。",
    "options": [],
    "answer": "1:4",
    "explanation": "串联总电阻2R，并联总电阻R/2，Q=U²t/R总，所以Q串/Q并=(R/2)/(2R)=1/4。"
  },
  {
    "id": 161,
    "type": "choice",
    "question": "家庭电路的电压是？",
    "options": [
      "A. 36V",
      "B. 110V",
      "C. 220V",
      "D. 380V"
    ],
    "answer": "C",
    "explanation": "我国家庭电路电压为220V。"
  },
  {
    "id": 162,
    "type": "choice",
    "question": "家庭电路中，保险丝的作用是？",
    "options": [
      "A. 保护用电器",
      "B. 在电流过大时自动切断电路",
      "C. 降低电压",
      "D. 增大电流"
    ],
    "answer": "B",
    "explanation": "保险丝在电流过大时熔断，自动切断电路，保护电路。"
  },
  {
    "id": 163,
    "type": "choice",
    "question": "家庭电路中，开关应该接在？",
    "options": [
      "A. 零线上",
      "B. 火线与用电器之间",
      "C. 地线上",
      "D. 任意位置"
    ],
    "answer": "B",
    "explanation": "开关应接在火线与用电器之间，断开后用电器不带电。"
  },
  {
    "id": 164,
    "type": "choice",
    "question": "家庭电路中，各用电器之间的连接方式是？",
    "options": [
      "A. 串联",
      "B. 并联",
      "C. 混联",
      "D. 不确定"
    ],
    "answer": "B",
    "explanation": "家庭电路中各用电器并联，互不影响。"
  },
  {
    "id": 165,
    "type": "choice",
    "question": "三孔插座中，中间的孔接的是？",
    "options": [
      "A. 火线",
      "B. 零线",
      "C. 地线",
      "D. 电话线"
    ],
    "answer": "C",
    "explanation": "三孔插座中间的孔接地线。"
  },
  {
    "id": 166,
    "type": "choice",
    "question": "使用测电笔时，手应该接触？",
    "options": [
      "A. 笔尖金属体",
      "B. 笔尾金属体",
      "C. 笔杆绝缘部分",
      "D. 不能触碰任何部分"
    ],
    "answer": "B",
    "explanation": "使用测电笔时，手应接触笔尾金属体，使电路形成通路。"
  },
  {
    "id": 167,
    "type": "choice",
    "question": "测电笔接触火线时，氖管会？",
    "options": [
      "A. 不发光",
      "B. 发光",
      "C. 变暗",
      "D. 闪烁"
    ],
    "answer": "B",
    "explanation": "测电笔接触火线时氖管发光。"
  },
  {
    "id": 168,
    "type": "choice",
    "question": "下列做法中符合安全用电原则的是？",
    "options": [
      "A. 用湿手拔插头",
      "B. 在高压线附近放风筝",
      "C. 使用绝缘皮破损的导线",
      "D. 不接触低压带电体，不靠近高压带电体"
    ],
    "answer": "D",
    "explanation": "不接触低压带电体、不靠近高压带电体是安全用电的基本原则。"
  },
  {
    "id": 169,
    "type": "choice",
    "question": "发现有人触电时，正确的做法是？",
    "options": [
      "A. 直接用手拉开触电者",
      "B. 立即切断电源或用干燥木棒挑开电线",
      "C. 大声呼救等待",
      "D. 用铁棒挑开电线"
    ],
    "answer": "B",
    "explanation": "发现触电应立即切断电源，或用干燥绝缘物挑开电线。"
  },
  {
    "id": 170,
    "type": "choice",
    "question": "家庭电路中电流过大的原因可能是？",
    "options": [
      "A. 电压过低",
      "B. 用电器功率过大或短路",
      "C. 电阻过大",
      "D. 开关接触不良"
    ],
    "answer": "B",
    "explanation": "家庭电路电流过大的原因：一是短路，二是用电器总功率过大。"
  },
  {
    "id": 171,
    "type": "choice",
    "question": "保险丝应该用什么材料制作？",
    "options": [
      "A. 铜丝",
      "B. 铁丝",
      "C. 电阻率大、熔点低的合金",
      "D. 电阻率小、熔点高的合金"
    ],
    "answer": "C",
    "explanation": "保险丝用电阻率大、熔点低的合金制作，电流过大时容易熔断。"
  },
  {
    "id": 172,
    "type": "choice",
    "question": "不能用铜丝代替保险丝的原因是？",
    "options": [
      "A. 铜丝电阻太小",
      "B. 铜丝熔点太高，电流过大时不会熔断",
      "C. 铜丝太贵",
      "D. 铜丝太软"
    ],
    "answer": "B",
    "explanation": "铜丝熔点高，电流过大时不会熔断，起不到保护作用。"
  },
  {
    "id": 173,
    "type": "choice",
    "question": "家庭电路中，电能表的作用是？",
    "options": [
      "A. 测量电流",
      "B. 测量电压",
      "C. 测量电功率",
      "D. 测量消耗的电能"
    ],
    "answer": "D",
    "explanation": "电能表用来测量电路中消耗的电能。"
  },
  {
    "id": 174,
    "type": "fill",
    "question": "家庭电路由进户线、___、总开关、保险装置和用电器组成。",
    "options": [],
    "answer": "电能表",
    "explanation": "家庭电路依次由进户线、电能表、总开关、保险装置、用电器组成。"
  },
  {
    "id": 175,
    "type": "fill",
    "question": "进户线有两根，一根叫___线，一根叫___线。",
    "options": [],
    "answer": "火；零",
    "explanation": "进户线分火线和零线。"
  },
  {
    "id": 176,
    "type": "fill",
    "question": "火线和零线之间的电压为___V，零线和地线之间的电压为___V。",
    "options": [],
    "answer": "220；0",
    "explanation": "火线与零线间电压220V，零线与地线间电压0V。"
  },
  {
    "id": 177,
    "type": "fill",
    "question": "辨别火线和零线可以用___。",
    "options": [],
    "answer": "测电笔",
    "explanation": "用测电笔辨别火线和零线。"
  },
  {
    "id": 178,
    "type": "fill",
    "question": "测电笔接触火线时氖管___，接触零线时氖管___。",
    "options": [],
    "answer": "发光；不发光",
    "explanation": "测电笔接触火线发光，接触零线不发光。"
  },
  {
    "id": 179,
    "type": "fill",
    "question": "家庭电路中引起电流过大的原因：一是___，二是用电器总功率过大。",
    "options": [],
    "answer": "短路",
    "explanation": "电流过大的原因是短路或用电器总功率过大。"
  },
  {
    "id": 180,
    "type": "fill",
    "question": "安全用电的原则是：不接触___带电体，不靠近___带电体。",
    "options": [],
    "answer": "低压；高压",
    "explanation": "安全用电原则：不接触低压带电体，不靠近高压带电体。"
  },
  {
    "id": 181,
    "type": "fill",
    "question": "三孔插座接线的原则是：左零右火___接地。",
    "options": [],
    "answer": "中间（或上）",
    "explanation": "三孔插座接线原则：左零右火中间接地。"
  },
  {
    "id": 182,
    "type": "fill",
    "question": "螺口灯泡的螺旋套应接___线，顶部金属块应接___线。",
    "options": [],
    "answer": "零；火",
    "explanation": "螺口灯泡螺旋套接零线，顶部金属块接火线，更换灯泡时更安全。"
  },
  {
    "id": 183,
    "type": "fill",
    "question": "家庭电路中，开关应与用电器___联，且接在___线上。",
    "options": [],
    "answer": "串；火",
    "explanation": "开关与用电器串联，且接在火线上。"
  },
  {
    "id": 184,
    "type": "fill",
    "question": "电能表的单位是___，俗称___。",
    "options": [],
    "answer": "kW·h；度",
    "explanation": "电能表单位kW·h，俗称度。"
  },
  {
    "id": 185,
    "type": "fill",
    "question": "家庭电路中同时使用的用电器越多，总电阻越___，干路电流越___。",
    "options": [],
    "answer": "小；大",
    "explanation": "并联用电器越多，总电阻越小，由I=U/R知干路电流越大。"
  },
  {
    "id": 186,
    "type": "choice",
    "question": "用电流表测电流时，电流表应该与被测电路？",
    "options": [
      "A. 串联",
      "B. 并联",
      "C. 串联或并联都行",
      "D. 以上都不对"
    ],
    "answer": "A",
    "explanation": "电流表应与被测电路串联。"
  },
  {
    "id": 187,
    "type": "choice",
    "question": "用电压表测电压时，电压表应该与被测电路？",
    "options": [
      "A. 串联",
      "B. 并联",
      "C. 串联或并联都行",
      "D. 以上都不对"
    ],
    "answer": "B",
    "explanation": "电压表应与被测电路并联。"
  },
  {
    "id": 188,
    "type": "choice",
    "question": "电流表使用时，电流应从哪个接线柱流入？",
    "options": [
      "A. 负接线柱",
      "B. 正接线柱",
      "C. 任意接线柱",
      "D. 接地柱"
    ],
    "answer": "B",
    "explanation": "电流应从电流表的正接线柱流入，负接线柱流出。"
  },
  {
    "id": 189,
    "type": "choice",
    "question": "电流表使用时绝对不允许的是？",
    "options": [
      "A. 串联在电路中",
      "B. 选择合适的量程",
      "C. 不经过用电器直接接在电源两极",
      "D. 让电流从正接线柱流入"
    ],
    "answer": "C",
    "explanation": "电流表绝对不允许不经过用电器直接接在电源两极，会造成短路烧毁电流表。"
  },
  {
    "id": 190,
    "type": "choice",
    "question": "伏安法测电阻的原理是？",
    "options": [
      "A. R=U/I",
      "B. R=I/U",
      "C. R=UI",
      "D. R=U²I"
    ],
    "answer": "A",
    "explanation": "伏安法测电阻的原理是R=U/I，用电压表测U，电流表测I。"
  },
  {
    "id": 191,
    "type": "choice",
    "question": "伏安法测电阻时，为了减小误差应？",
    "options": [
      "A. 只测一次",
      "B. 多次测量取平均值",
      "C. 只测电压",
      "D. 只测电流"
    ],
    "answer": "B",
    "explanation": "多次测量取平均值可以减小误差。"
  },
  {
    "id": 192,
    "type": "choice",
    "question": "在探究电流与电压的关系实验中，需要控制什么不变？",
    "options": [
      "A. 电压",
      "B. 电流",
      "C. 电阻",
      "D. 电功率"
    ],
    "answer": "C",
    "explanation": "探究电流与电压关系时，需控制电阻不变。"
  },
  {
    "id": 193,
    "type": "choice",
    "question": "在探究电流与电阻的关系实验中，更换较大电阻后，滑动变阻器的滑片应向哪移动？",
    "options": [
      "A. 阻值减小的方向",
      "B. 阻值增大的方向",
      "C. 不需要移动",
      "D. 任意方向"
    ],
    "answer": "B",
    "explanation": "更换较大电阻后，为保持定值电阻两端电压不变，需增大滑动变阻器接入阻值。"
  },
  {
    "id": 194,
    "type": "fill",
    "question": "电流表应与被测电路___联，电压表应与被测电路___联。",
    "options": [],
    "answer": "串；并",
    "explanation": "电流表串联，电压表并联。"
  },
  {
    "id": 195,
    "type": "fill",
    "question": "电流表和电压表使用时，都应让电流从___接线柱流入，从___接线柱流出。",
    "options": [],
    "answer": "正；负",
    "explanation": "电流都应从正接线柱流入，负接线柱流出。"
  },
  {
    "id": 196,
    "type": "fill",
    "question": "在使用电流表和电压表前，应先___量程，使指针指在刻度盘的___附近，以减小误差。",
    "options": [],
    "answer": "选择（或试触）；满刻度（或最大值）",
    "explanation": "应选择合适的量程，使指针偏转较大，便于读数且减小误差。"
  },
  {
    "id": 197,
    "type": "fill",
    "question": "伏安法测电阻的实验原理是___。",
    "options": [],
    "answer": "R=U/I",
    "explanation": "伏安法测电阻原理R=U/I。"
  },
  {
    "id": 198,
    "type": "fill",
    "question": "伏安法测电阻时，滑动变阻器的作用是___和___。",
    "options": [],
    "answer": "保护电路；改变被测电阻两端的电压和电流",
    "explanation": "滑动变阻器的作用是保护电路和改变电压电流以多次测量。"
  },
  {
    "id": 199,
    "type": "fill",
    "question": "在探究电流与电压的关系实验中，采用的研究方法是___法。",
    "options": [],
    "answer": "控制变量",
    "explanation": "探究电流与电压关系时采用控制变量法，控制电阻不变。"
  },
  {
    "id": 200,
    "type": "fill",
    "question": "在测量小灯泡电功率的实验中，实验原理是___。",
    "options": [],
    "answer": "P=UI",
    "explanation": "测量小灯泡电功率的原理是P=UI。"
  }
];