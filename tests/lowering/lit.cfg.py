import lit.formats

config.name = "lowering"
config.test_format = lit.formats.ShTest(True)
config.suffixes = ['.ina']
config.substitutions.append(("%ina", os.path.join("../../bin", "ina")))
