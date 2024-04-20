import rekekssion

model = rekekssion.fit(
    data = rekekssion.data.concrete,
    y = "compressive_strength",
    x = ["cement", "water", "age"],
    intercept = False,
)
model.summary()
model.plot()
