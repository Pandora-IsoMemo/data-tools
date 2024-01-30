#' Set up the authentication with your API key
#'
#' @description Access to GPT-3's functions requires an API key that you obtain
#'   from [https://openai.com/api/](https://openai.com/api/).
#'   `gpt3_authenticate()` looks for your API key in a file that you provide the
#'   path to and ensures you can connect to the models. `gpt3_endsession()`
#'   overwrites your API key _for this session_ (it is recommended that you run
#'   this when you are done). `check_apikey_form()` is a simple check if any
#'   information has been provided at all.
#' @param path The file path to the API key
#' @return A confirmation message
#' @details The easiest way to store you API key is in a `.txt` file with _only_
#'   the API key in it (without quotation marks or other common string
#'   indicators). `gpt3_authenticate()` reads the single file you point it to
#'   and retrieves the content as authentication key for all requests.
gpt3_authenticate = function(path){
  apikey_ = readLines(path)
  pkg.env$api_key = apikey_
  print(paste0("Will use --> ", pkg.env$api_key, " for authentication."))
}

gpt3_endsession = function(){
  pkg.env$api_key = "---"
  print('-- session ended: you need to re-authenticate again next time.')
}

check_apikey_form = function(){

  if(exists(x = 'pkg.env$api_key') == FALSE){
    warning("Use gpt3_authenticate() to set your API key")
  } else if(nchar(pkg.env$api_key) < 10){

    warning("Use gpt3_authenticate() to set your API key")

  }
}

#'Makes a single completion request to the GPT-3 API
#'
#'@description `gpt3_single_completion()` sends a single [completion
#'  request](https://beta.openai.com/docs/api-reference/completions) to the Open
#'  AI GPT-3 API.
#'@details For a general guide on the completion requests, see
#'  [https://beta.openai.com/docs/guides/completion](https://beta.openai.com/docs/guides/completion).
#'   This function provides you with an R wrapper to send requests with the full
#'  range of request parameters as detailed on
#'  [https://beta.openai.com/docs/api-reference/completions](https://beta.openai.com/docs/api-reference/completions)
#'   and reproduced below.
#'
#'  For the `best_of` parameter: When used with n, best_of controls the number
#'  of candidate completions and n specifies how many to return – best_of must
#'  be greater than n. Note that this is handled by the wrapper automatically
#'  if(best_of <= n){ best_of = n}.
#'
#'  Parameters not included/supported: - `logit_bias`:
#'  [https://beta.openai.com/docs/api-reference/completions/create#completions/create-logit_bias](https://beta.openai.com/docs/api-reference/completions/create#completions/create-logit_bias)
#'   - `echo`:
#'  [https://beta.openai.com/docs/api-reference/completions/create#completions/create-echo](https://beta.openai.com/docs/api-reference/completions/create#completions/create-echo)
#'   - `stream`:
#'  [https://beta.openai.com/docs/api-reference/completions/create#completions/create-stream](https://beta.openai.com/docs/api-reference/completions/create#completions/create-stream)
#'
#'
#'@param prompt_input character that contains the prompt to the GPT-3 request
#'@param model a character vector that indicates the
#'  [model](https://beta.openai.com/docs/models/gpt-3) to use; one of
#'  "text-davinci-003" (default), "text-davinci-002", "text-davinci-001",
#'  "text-curie-001", "text-babbage-001" or "text-ada-001"
#'@param output_type character determining the output provided: "complete"
#'  (default), "text" or "meta"
#'@param suffix character (default: NULL) (from the official API documentation:
#'  _The suffix that comes after a completion of inserted text_)
#'@param max_tokens numeric (default: 100) indicating the maximum number of
#'  tokens that the completion request should return (from the official API
#'  documentation: _The maximum number of tokens to generate in the completion.
#'  The token count of your prompt plus max_tokens cannot exceed the model's
#'  context length. Most models have a context length of 2048 tokens (except for
#'  the newest models, which support 4096)_)
#'@param temperature numeric (default: 0.9) specifying the sampling strategy of
#'  the possible completions (from the official API documentation: _What
#'  sampling temperature to use. Higher values means the model will take more
#'  risks. Try 0.9 for more creative applications, and 0 (argmax sampling) for
#'  ones with a well-defined answer. We generally recommend altering this or
#'  top_p but not both._)
#'@param top_p numeric (default: 1) specifying sampling strategy as an
#'  alternative to the temperature sampling (from the official API
#'  documentation: _An alternative to sampling with temperature, called nucleus
#'  sampling, where the model considers the results of the tokens with top_p
#'  probability mass. So 0.1 means only the tokens comprising the top 10%
#'  probability mass are considered. We generally recommend altering this or
#'  temperature but not both._)
#'@param n numeric (default: 1) specifying the number of completions per request
#'  (from the official API documentation: _How many completions to generate for
#'  each prompt. **Note: Because this parameter generates many completions, it
#'  can quickly consume your token quota.** Use carefully and ensure that you
#'  have reasonable settings for max_tokens and stop._)
#'@param logprobs numeric (default: NULL) (from the official API documentation:
#'  _Include the log probabilities on the logprobs most likely tokens, as well
#'  the chosen tokens. For example, if logprobs is 5, the API will return a list
#'  of the 5 most likely tokens. The API will always return the logprob of the
#'  sampled token, so there may be up to logprobs+1 elements in the response.
#'  The maximum value for logprobs is 5. If you need more than this, please go
#'  to [https://help.openai.com/en/](https://help.openai.com/en/) and describe
#'  your use case._)
#'@param stop character or character vector (default: NULL) that specifies after
#'  which character value when the completion should end (from the official API
#'  documentation: _Up to 4 sequences where the API will stop generating further
#'  tokens. The returned text will not contain the stop sequence._)
#'@param presence_penalty numeric (default: 0) between -2.00  and +2.00 to
#'  determine the penalisation of repetitiveness if a token already exists (from
#'  the official API documentation: _Number between -2.0 and 2.0. Positive
#'  values penalize new tokens based on whether they appear in the text so far,
#'  increasing the model's likelihood to talk about new topics._). See also:
#'  [https://beta.openai.com/docs/api-reference/parameter-details](https://beta.openai.com/docs/api-reference/parameter-details)
#'
#'@param frequency_penalty numeric (default: 0) between -2.00  and +2.00 to
#'  determine the penalisation of repetitiveness based on the frequency of a
#'  token in the text already (from the official API documentation: _Number
#'  between -2.0 and 2.0. Positive values penalize new tokens based on their
#'  existing frequency in the text so far, decreasing the model's likelihood to
#'  repeat the same line verbatim._). See also:
#'  [https://beta.openai.com/docs/api-reference/parameter-details](https://beta.openai.com/docs/api-reference/parameter-details)
#'
#'@param best_of numeric (default: 1) that determines the space of possibilities
#'  from which to select the completion with the highest probability (from the
#'  official API documentation: _Generates `best_of` completions server-side and
#'  returns the "best" (the one with the highest log probability per token)_).
#'  See details.
#'
#'@return A list with two data tables (if `output_type` is the default
#'  "complete"): `[[1]]` contains the data table with the columns `n` (= the mo.
#'  of `n` responses requested), `prompt` (= the prompt that was sent), and
#'  `gpt3` (= the completion as returned from the GPT-3 model). `[[2]]` contains
#'  the meta information of the request, including the request id, the
#'  parameters of the request and the token usage of the prompt
#'  (`tok_usage_prompt`), the completion (`tok_usage_completion`) and the total
#'  usage (`tok_usage_total`).
#'
#'  If `output_type` is "text", only the data table in slot `[[1]]` is returned.
#'
#'  If `output_type` is "meta", only the data table in slot `[[2]]` is returned.
gpt3_single_completion = function(prompt_input
                                  , model = 'text-davinci-003'
                                  , output_type = 'complete'
                                  , suffix = NULL
                                  , max_tokens = 100
                                  , temperature = 0.9
                                  , top_p = 1
                                  , n = 1
                                  , logprobs = NULL
                                  , stop = NULL
                                  , presence_penalty = 0
                                  , frequency_penalty = 0
                                  , best_of = 1){

  #check for request issues with `n` and `best_of`
  if(best_of < n){
    best_of = n
    message('To avoid an `invalid_request_error`, `best_of` was set to equal `n`')
  }

  if(temperature == 0 & n > 1){
    n = 1
    message('You are running the deterministic model, so `n` was set to 1 to avoid unnecessary token quota usage.')
  }

  parameter_list = list(prompt = prompt_input
                        , model = model
                        , suffix = suffix
                        , max_tokens = max_tokens
                        , temperature = temperature
                        , top_p = top_p
                        , n = n
                        , logprobs = logprobs
                        , stop = stop
                        , presence_penalty = presence_penalty
                        , frequency_penalty = frequency_penalty
                        , best_of = best_of)

  request_base = httr::POST(url = url.completions
                            , body = parameter_list
                            , httr::add_headers(Authorization = paste("Bearer", pkg.env$api_key))
                            , encode = "json")

  request_content = httr::content(request_base)
  # request_content = httr::content(request_base, encoding = "Latin-ASCII")

  if(n == 1){
    core_output = data.table::data.table('n' = 1
                                         , 'prompt' = prompt_input
                                         , 'gpt3' = request_content$choices[[1]]$text)
  } else if(n > 1){

    core_output = data.table::data.table('n' = 1:n
                                         , 'prompt' = rep(prompt_input, n)
                                         , 'gpt3' = rep("", n))

    for(i in 1:n){
      core_output$gpt3[i] = request_content$choices[[i]]$text
    }

  }


  meta_output = data.table::data.table('request_id' = request_content$id
                                       , 'object' = request_content$object
                                       , 'model' = request_content$model
                                       , 'param_prompt' = prompt_input
                                       , 'param_model' = model
                                       , 'param_suffix' = suffix
                                       , 'param_max_tokens' = max_tokens
                                       , 'param_temperature' = temperature
                                       , 'param_top_p' = top_p
                                       , 'param_n' = n
                                       , 'param_logprobs' = logprobs
                                       , 'param_stop' = stop
                                       , 'param_presence_penalty' = presence_penalty
                                       , 'param_frequency_penalty' = frequency_penalty
                                       , 'param_best_of' = best_of
                                       , 'tok_usage_prompt' = request_content$usage$prompt_tokens
                                       , 'tok_usage_completion' = request_content$usage$completion_tokens
                                       , 'tok_usage_total' = request_content$usage$total_tokens)

  if(output_type == 'complete'){
    output = list(core_output
                  , meta_output)
  } else if(output_type == 'meta'){
    output = meta_output
  } else if(output_type == 'text'){
    output = core_output
  }

  return(output)

}

#' Make a test request to the GPT-3 API
#'
#' @description `gpt3_test_completion()` sends a basic [completion
#'   request](https://beta.openai.com/docs/api-reference/completions) to the
#'   Open AI GPT-3 API.
#' @param verbose (boolean) if TRUE prints the actual prompt and GPT-3
#'   completion of the test request (default: TRUE).
#' @return A message of success or failure of the connection.
gpt3_test_completion = function(verbose=TRUE){

  check_apikey_form()

  test_prompt = 'Write a story about R Studio: '
  test_output = gpt3_single_completion(prompt_input = test_prompt
                                       , max_tokens = 100)
  print(paste0('.. test successful ..'))

  if(verbose==TRUE){
    # print(paste0('Requested completion for this prompt --> ', test_prompt))
    # print(paste0('GPT-3 completed --> ', test_output))
    test_output
  }

}
