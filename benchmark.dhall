let conf = ./spago.dhall
in conf // {
  sources = conf.sources # [ "benchmark/**/*.purs" ],
  backend = "purs-backend-es build",
  dependencies = conf.dependencies # [ "console", "minibench", "effect", "object-maps" ]
}