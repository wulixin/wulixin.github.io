# Project Memory — wulixin.github.io

## 项目概述
- 武利鑫个人数据科学与量化金融作品集网站（GitHub Pages）
- 技术栈：原生 HTML/CSS/JS + WebGL（银河粒子背景，8000 粒子 GLSL 着色器）
- 内容：R 包技能汇总（15 领域 300+ 包）、Python 开源生态分析、金融分析师技能体系、教育软件、QuandlFinance 投研 Agent

## 设计规范
- 深色科技风配色：背景 #060810，蓝紫金渐变点缀
- 字体：Inter + Noto Sans SC
- 卡片采用毛玻璃效果 + 渐变边框（CSS mask-composite）
- 滚动渐入用 IntersectionObserver，统计数字有滚动动画
- 移动端汉堡菜单

## 关键文件
- index.html — 主页（2026-08-22 重新设计）
- R语言时间序列包技能汇总/ — 15 个 HTML 技能卡
- Python开源技术分享/ — 4 个生态分析
- 期货与衍生品分析师必备技能/ — 3 套金融课程
- 初中生背单词/、初中物理常识训练/ — 教育软件
- QuandlFinance/ — 投研 Agent 三页面

## 本地预览
- `python3 -m http.server 8765` 然后访问 http://localhost:8765
