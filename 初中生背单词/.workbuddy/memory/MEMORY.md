# 项目记忆：初中生背单词

## 项目概况
- 目标用户：初中生，需要趣味化英语词汇学习
- 主文件：`index.html`（单文件应用）

## 设计原则
- 游戏化激励：XP + Combo连击 + 宝石 + 连续打卡
- 每题必有解析：正确答案 + 语法要点 + 例句 + 记忆口诀
- 按难度渐进：初级→中级→高级→四级→六级

## 数据结构（题目对象）
```js
{
  id, cat, diff, type,   // type: 'mc' | 'fill'
  q,                      // 题目文本（空格用___表示）
  hint,                   // 提示（可选）
  options,                // MC选项数组
  answer,                 // MC: index; fill: string
  explain,                // 语法解析文字
  examples,               // 例句数组（含<strong>高亮）
  mnemonic               // 记忆口诀
}
```

## 题目覆盖范围（45题）
- preposition 介词：6题（on/at/in/since/over/despite）
- article 冠词：4题（a/an/the/0冠词）
- noun 名词：4题（不可数名词/名词形式/复合名词/近形词）
- verb 动词：5题（三单/进行时/完成时/过完时/虚拟语气）
- tense_present 现在时：2题
- tense_past 过去时：3题
- tense_future 将来时：2题
- adjective 形容词：3题
- phrase 短语：7题
- collocation 词语搭配：9题（含CET4/CET6）

## 后续扩展方向
- 扩充题目到200+题（目标1000题）
- 添加"本周主题"功能（如介词周）
- 加入语音TTS朗读功能
- 家长端查看学习报告
