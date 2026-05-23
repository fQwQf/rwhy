# rwhy

> **你的 AI R 语言编程助手。**
> 解释报错、生成代码、即时帮助——无需离开 R 控制台。

`rwhy` 是一个将大语言模型 (LLM) 的能力直接带入 R 工作流的 R 包。专为 R 初学者、学生和所有曾被红色报错信息困扰的人设计——看到报错，问一句 *why?*

## 特性

- **`why()`** — 解释上一次报错的原因和修复方法。报错后立即调用。
- **`ask_r("...")`** — 用自然语言描述需求，AI 生成可运行的 R 代码。
- **`watch_on()` / `watch_off()`** — 全局自动监听，报错瞬间 AI 自动解释。
- **RStudio 深度集成** — 可将 AI 生成的代码直接插入脚本编辑器。
- **自动中英文适配** — 根据系统语言自动切换界面语言。
- **OpenAI 兼容** — 支持 DeepSeek、智谱 GLM、通义千问、Kimi、OpenAI 等。

## 安装

```r
# 从 GitHub 安装
# install.packages("devtools")
devtools::install_github("your_username/rwhy")
```

## 快速开始

### 方式一：交互式配置向导（推荐）

```r
library(rwhy)
configure()   # 启动交互式向导：选服务商 → 选模型 → 粘贴 API Key，一步到位
```

### 方式二：一行命令快速配置

```r
# 使用智谱 GLM（支持 GLM Coding Plan）
use_glm()                          # 切换到 glm-4-flash
set_ai_key("你的智谱API密钥")       # 一次性配置，永久保存

# 或 DeepSeek（默认）
use_deepseek()
set_ai_key("sk-your-deepseek-key")

# 或其他服务商
use_qwen()      # 通义千问（阿里云）
use_kimi()      # Moonshot AI
use_openai()    # OpenAI
use_ollama()    # 本地模型（免费，无需 API Key）
```

## 使用示例

### 解释报错

```r
> 1 + "a"
Error in 1 + "a" : non-numeric argument to binary operator

> why()
── rwhy ──────────────────────────────────────
- **报错原因**: 不能将数字和字符串做加法运算。
- **根本原因**: `+` 运算符要求两边都是数值类型。`"a"` 是字符型，R 无法计算 `1 + "a"`。
- **修复方案**:
  ```r
  # 先转换为数值
  1 + as.numeric("1")  # 正常运行
  ```
── rwhy ──────────────────────────────────────
```

### 自然语言生成代码

```r
> ask_r("用 mtcars 数据集画一个 mpg 和 wt 的散点图，加上蓝色趋势线")
AI 正在为您编写代码：用 mtcars 数据集画一个 mpg 和 wt 的散点图...

── 生成的代码 ─────────────────────────────────
library(ggplot2)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "MPG vs Weight",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon") +
  theme_minimal()
── 生成的代码 ─────────────────────────────────

是否运行该代码？[y/N/e(=插入到脚本)]: y
正在运行代码...
```

### 自动报错监听

```r
> watch_on()
rwhy 自动监控已**开启**。报错将被自动解释。

> log("abc")
Error in log("abc") : non-numeric argument to mathematical function
── rwhy 自动解释 ──────────────────────────────
`log()` 需要数值输入。"abc" 是字符串。
修复：`log(as.numeric("123"))` 或确保变量是数值型。
── rwhy 自动解释 ──────────────────────────────

> watch_off()
rwhy 自动监控已**关闭**。
```

### 插入代码到脚本（RStudio）

```r
ask_r("筛选 mtcars 中气缸数大于 4 的车并按 mpg 排序", insert = TRUE)
# 代码直接插入到 RStudio 编辑器光标位置
```

## 支持的服务商

| 服务商 | 函数 | 默认模型 | 费用 |
|---|---|---|---|
| **DeepSeek** | `use_deepseek()` | `deepseek-chat` | 极便宜 |
| **智谱 GLM** | `use_glm()` | `glm-4-flash` | GLM Coding Plan |
| **通义千问** | `use_qwen()` | `qwen-turbo` | 有免费额度 |
| **Kimi** | `use_kimi()` | `moonshot-v1-8k` | 有免费额度 |
| **OpenAI** | `use_openai()` | `gpt-4o-mini` | 较贵 |
| **Ollama (本地)** | `use_ollama()` | `llama3` | 免费，无需 Key |

指定模型：`use_glm(model = "glm-4-plus")`

自定义端点：
```r
ai_config(base_url = "https://your-endpoint/v1/chat/completions", model = "your-model")
```

## 语言切换

`rwhy` 根据系统语言自动适配（中文系统显示中文，英文系统显示英文）。

手动切换：
```r
rwhy_lang("zh")     # 强制中文
rwhy_lang("en")     # 强制英文
rwhy_lang("auto")   # 恢复自动检测
rwhy_lang()         # 查看当前语言
```

> AI 回复的语言也会跟随设置自动调整。

## 包结构

```
rwhy/
├── R/
│   ├── i18n.R          # 国际化引擎（自动检测语言 + 翻译数据库）
│   ├── aaa-api.R       # API 密钥管理 & 服务商预设
│   ├── ask_llm.R       # LLM API 调用核心
│   ├── extract_code.R  # Markdown 代码块提取器
│   ├── why.R           # 报错解释 & 自动监控
│   ├── ask_r.R         # 代码生成 & 执行
│   └── utils.R         # RStudio 上下文 & 启动消息
├── man/                # 文档
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## 系统要求

- R >= 4.0.0
- 依赖包：`httr2`、`jsonlite`、`cli`
- 可选：`rstudioapi`（RStudio 集成功能）
- 任意 OpenAI 兼容 API 的密钥
