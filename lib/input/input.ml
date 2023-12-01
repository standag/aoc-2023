let read_input (name : string) : string =
  In_channel.with_open_text
    ("/Users/standag/p/github/aoc-2023/inputs/" ^ name)
    In_channel.input_all
