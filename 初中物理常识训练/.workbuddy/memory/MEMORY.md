# MEMORY.md - 初中物理常识训练项目

## 项目概述
- **项目名称**: 初中物理常识训练 · 1000题
- **类型**: 单页HTML交互式答题系统
- **目的**: 初中物理知识入门强化训练，即做即改即解析
- **创建日期**: 2026-05-28

## 技术架构
- 单页HTML应用（index.html）+ 8个数据JS文件（data/目录）
- 无后端依赖，纯前端运行
- LocalStorage持久化进度、错题、收藏
- 键盘快捷键：A/B/C/D选择，←→翻页

## 数据文件
| 文件 | 板块 | 题数 | 变量名 |
|------|------|------|--------|
| sound.js | 声学 | 130 | SOUND_QUESTIONS |
| optics.js | 光学 | 130 | OPTICS_QUESTIONS |
| heat.js | 热学 | 130 | HEAT_QUESTIONS |
| mechanics.js | 力学 | 200 | MECHANICS_QUESTIONS |
| electricity.js | 电学 | 200 | ELECTRICITY_QUESTIONS |
| magnetism.js | 磁学 | 70 | MAGNETISM_QUESTIONS |
| energy.js | 能源信息 | 70 | ENERGY_QUESTIONS |
| experiment.js | 实验探究 | 70 | EXPERIMENT_QUESTIONS |

## 功能特性
- 8大板块筛选 + 题型筛选（选择/填空）
- 做完即显对错+解析
- 错题本自动收录
- 收藏功能
- 正确率统计
- 进度保存（LocalStorage）
- 练习/测验两种模式

## 注意事项
- 数据文件编码必须为UTF-8
- 部分数据文件使用JS对象格式（id: 1），部分使用JSON格式（"id": 1），两种格式浏览器均可正常加载
