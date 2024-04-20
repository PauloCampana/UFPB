## Rekekssion
The "I'm lazy and just want results" linear regression package.

```py
import rekekssion

model = rekekssion.fit(
    data = rekekssion.data.concrete,
    y = "compressive_strength",
    x = ["cement", "water", "age"],
    intercept = True,
)
print(model)
plot(model)

```

<!-- TODO: show output here -->
