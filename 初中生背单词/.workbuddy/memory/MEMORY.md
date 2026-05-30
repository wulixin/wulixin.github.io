# 项目记忆：初中生背单词

## 项目概况
- 目标用户：初中生，需要趣味化英语词汇学习
- 主文件：`index.html`（单文件应用，含 HTML+CSS+JS）
- 题库文件：`data/*.json`（10 个类别，共 5273 题，去重后）

## 架构
- **动态加载**：`index.html` 通过 `fetch()` 加载 `data/` 下的 JSON 题库文件
- **去重机制**：基于 `q + cat + type` 三元组跨文件去重
- **回退方案**：HTML 内置 `LEGACY_QUESTIONS` 数组，JSON 加载失败时使用
- **CAT_FILE_MAP**：将类别映射到 JSON 文件名（如 `'preposition' → 'prepositions.json'`）

## 题库文件（`data/` 目录）
| 文件 | 题数 | 覆盖内容 |
|------|------|----------|
| prepositions.json | 672 | 时间介词、空间介词、动介搭配、形介搭配 |
| articles.json | 631 | a/an、the、零冠词、固定搭配、综合辨析 |
| nouns.json | 418 | 不可数名词、不规则复数、集合名词、所有格 |
| verbs.json | 559 | 不规则动词、短语动词、动词辨析 |
| adjectives.json | 379 | 形容词辨析、比较级、形容词+介词 |
| tenses.json | 779 | 8 种时态：一般现在/现在进行/一般过去/过去进行/现在完成/过去完成/一般将来/将来进行 |
| phrases.json | 259 | 动词短语、介词短语 |
| collocations.json | 202 | 词语搭配 |
| cet4.json | 687 | CET4 核心词汇 + 短语动词搭配 |
| cet6.json | 687 | CET6 进阶词汇 + 搭配 |

**总计：5273 题**

## 生成脚本
- `generate_questions.py` — 介词生成器（原始，生成 prepositions.json + articles.json 基础）
- `expand_articles.py` — 冠词扩展器（117→631）
- `expand_tenses.py` — 时态扩展器（296→779）
- `gen_cet.py` — CET4/CET6 词汇生成器
- `gen_all.py` / `gen_nouns.py` / `gen_verbs_tenses.py` / `gen_adjectives.py` / `gen_phrases.py` — 其他类别

所有生成脚本使用 Python 3.13.12（managed runtime）：
`/Users/wulixin/.workbuddy/binaries/python/versions/3.13.12/bin/python3`

## 数据结构（题目对象）
```js
{
  id: number,
  cat: string,       // 'preposition'|'article'|'noun'|'verb'|'adjective'|
                     // 'tense_present'|'tense_past'|'tense_future'|
                     // 'phrase'|'collocation'|'cet4'|'cet6'
  diff: string,      // 'easy'|'medium'|'hard'
  type: string,      // 'mc'（选择）| 'fill'（填空）
  q: string,         // 题目文本（空位用___表示）
  hint: string,      // 提示
  options: string[], // MC 选项数组
  answer: number|string, // MC: index; fill: 答案文本
  explain: string,   // 解析
  examples: string[],// 例句（含 <strong> 高亮）
  mnemonic: string   // 记忆口诀
}
```

## 设计原则
- 游戏化激励：XP + Combo 连击 + 宝石 + 连续打卡
- 每题必有解析：正确答案 + 语法要点 + 例句 + 记忆口诀
- 按难度渐进：初级 → 中级 → 高级 → 四级 → 六级
- CET4/CET6 筛选通过 `q.cat === 'cet4'` 或 `q.cat === 'cet6'` 实现

## 后续扩展方向
- 添加"本周主题"功能（如介词周）
- 加入语音 TTS 朗读功能
- 家长端查看学习报告
- 可进一步扩充 collocations/phrases/adjectives 类别
