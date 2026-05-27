# rwhy

> 你的 AI R 语言编程助手。看到报错，问一句 `why()`。

`rwhy` 把 OpenAI 兼容的大语言模型接入 R 控制台：解释报错、生成 R 代码、自动监听错误，并支持 DeepSeek、智谱 GLM、通义千问、Kimi、OpenAI、Ollama 和自定义 OpenAI 兼容 API。

## 特性

- `why()`：解释上一次 R 报错，给出原因和修复代码。
- `ask_r()`：用自然语言生成 R 代码，可选择运行或插入 RStudio 脚本。
- `watch_on()` / `watch_off()`：自动监听 R 错误并解释。
- `set_ai_key()` / `configure()`：交互式选择服务商、模型和 API Key。
- `use_custom()`：接入自建网关、本地推理服务或第三方 OpenAI 兼容端点。
- `ai_status()`：查看当前 provider、model、key、语言和监听状态。
- 自动中英文适配。

## 安装

```r
# install.packages("devtools")
devtools::install_github("your_username/rwhy")
```

本地开发安装：

```bash
R CMD INSTALL /home/fq/project/rproject/rwhy
```

## 快速开始

```r
library(rwhy)

set_ai_key()
# 交互式流程：
# 1. 选择服务商/API
# 2. 选择模型
# 3. 粘贴 API Key

ai_status()
```

如果你已经知道服务商和 key，也可以脚本式配置：

```r
use_deepseek()
set_ai_key("sk-your-deepseek-key")

use_glm(model = "glm-4-flash")
set_ai_key("your-glm-key")
```

本地 Ollama 不需要 key：

```r
use_ollama(model = "llama3")
ai_status()
```

## 自定义 OpenAI 兼容 API

适用于自建 API 网关、本地模型服务、One API / New API / LiteLLM / vLLM / llama.cpp server 等兼容 `/v1/chat/completions` 的接口。

```r
use_custom(
  base_url = "https://yoururl.com/v1/chat/completions",
  model = "gpt-5.5",
  key = "sk-your-key"
)
```

`base_url` 也可以只写主机，`rwhy` 会自动补全 `/v1/chat/completions`：

```r
use_custom(
  base_url = "https://yoururl.com",
  model = "gpt-5.5",
  key = "sk-your-key"
)
```

如果是本地端口：

```r
use_custom(
  base_url = "http://localhost:8000",
  model = "your-model",
  key = "sk-local"
)
```

交互式 `set_ai_key()` 中选择 `Custom OpenAI-compatible` 后，可以输入完整 endpoint：

```text
https://yoururl.com/v1/chat/completions
```

也可以留空 endpoint，然后分别输入：

```text
Host: localhost
Port: 8000
Model name: your-model
```

## 先用 curl 验证端点

推荐先确认你的 OpenAI 兼容 API 是否能正常响应：

```bash
curl -i -X POST "https://yoururl.com/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer sk-your-key" \
  -d '{
    "model": "gpt-5.5",
    "stream": false,
    "messages": [
      {"role": "user", "content": "Reply with exactly: pong"}
    ],
    "temperature": 0
  }'
```

理想的非流式返回是 JSON：

```json
{
  "choices": [
    {
      "message": {
        "content": "pong"
      }
    }
  ]
}
```

有些兼容服务会把 `Content-Type` 标成 `text/event-stream`，但 body 实际仍是 JSON；`rwhy` 已兼容这种情况。真正的 SSE 流式响应也支持：

```text
data: {"choices":[{"delta":{"content":"pong"}}]}
data: [DONE]
```

## 使用示例

### 解释数据清洗错误

```r
df <- data.frame(
  group = c("A", "A", "B"),
  value = c("10", "bad", "20")
)

mean(df$value)
# Warning in mean.default(df$value) :
#   argument is not numeric or logical: returning NA

why()
```

`why()` 会解释为什么字符列不能直接求均值，并给出类似修复思路：

```r
df$value_num <- suppressWarnings(as.numeric(df$value))
mean(df$value_num, na.rm = TRUE)
```

### 生成一段实用分析代码

```r
ask_r(
  "用 mtcars 计算不同 cyl 的平均 mpg、平均 hp 和样本量，按平均 mpg 降序排列"
)
```

可能生成：

```r
library(dplyr)

mtcars %>%
  group_by(cyl) %>%
  summarise(
    mean_mpg = mean(mpg),
    mean_hp = mean(hp),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_mpg))
```

### 生成可视化

```r
ask_r(
  "用 ggplot2 画 mtcars 中 wt 和 mpg 的散点图，按 cyl 上色，加线性趋势线",
  insert = TRUE
)
```

在 RStudio 中，`insert = TRUE` 会把代码插入当前脚本光标位置，适合先审阅再运行。

### 自动解释错误

```r
watch_on()

log("abc")
# 出错后 rwhy 会自动请求 AI 解释

watch_off()
```

## 命令行使用

查看配置：

```bash
Rscript -e 'library(rwhy); ai_status()'
```

配置自定义 API：

```bash
Rscript -e 'library(rwhy); use_custom("https://yoururl.com", "gpt-5.5", key = "sk-your-key")'
```

生成代码但不执行：

```bash
Rscript -e 'library(rwhy); ask_r("读取 data.csv，按 status 分组汇总 amount 总和", auto_run = FALSE)'
```

解释同一 R 会话中的错误：

```bash
Rscript -e 'library(rwhy); try(log("abc")); why()'
```

## 支持的服务商

| 服务商 | 函数 | 默认模型 | 说明 |
|---|---|---|---|
| DeepSeek | `use_deepseek()` | `deepseek-chat` | 适合代码与通用问答 |
| 智谱 GLM | `use_glm()` | `glm-4-flash` | 支持 GLM Coding Plan |
| 通义千问 | `use_qwen()` | `qwen-turbo` | 阿里云 DashScope 兼容模式 |
| Kimi | `use_kimi()` | `moonshot-v1-8k` | 长上下文 |
| OpenAI | `use_openai()` | `gpt-4o-mini` | OpenAI 官方接口 |
| Ollama | `use_ollama()` | `llama3` | 本地模型，无需 key |
| 自定义接口 | `use_custom()` | 自行填写 | 任意 OpenAI 兼容 API |

## 常用配置

```r
ai_status()            # 查看当前配置
rwhy_lang("zh")        # 强制中文
rwhy_lang("en")        # 强制英文
rwhy_lang("auto")      # 自动检测语言
unset_ai_key()         # 清理当前会话和 ~/.Renviron 中的 key
```

## 包结构

```text
rwhy/
├── R/
│   ├── i18n.R          # 国际化
│   ├── aaa-api.R       # API 密钥、服务商、自定义端点
│   ├── ask_llm.R       # LLM API 调用和响应解析
│   ├── extract_code.R  # Markdown 代码块提取
│   ├── why.R           # 报错解释和自动监听
│   ├── ask_r.R         # 代码生成和安全执行
│   └── utils.R         # RStudio 上下文和工具函数
├── tests/
├── man/
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## 系统要求

- R >= 4.0.0
- 依赖包：`httr2`、`jsonlite`、`cli`
- 可选：`rstudioapi`，用于 RStudio 插入代码
- 一个 OpenAI 兼容 API，或本地 Ollama
