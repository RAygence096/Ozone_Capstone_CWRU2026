library(RAQSAPI)

# 验证 Key
RAQSAPI::aqs_credentials(
  username = Sys.getenv("AQS_USER"), 
  key = Sys.getenv("AQS_KEY")
)

# 测试：列出所有州（Metadata Call）
test_call <- aqs_states()
if (nrow(test_call) > 0) {
  message("API Key is VALID and working!")
} else {
  message("Credential Error: Key might be invalid.")
}