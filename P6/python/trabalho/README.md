## Rekekssion
The "I'm lazy and just want results" linear regression package.

```py
import rekekssion

data = rekekssion.data.concrete
model = rekekssion.fit(
    data = data,
    y = "compressive_strenght",
    x = ["cement", "water", "age"],
    intercept = True,
)
print(model)
```

<!-- TODO: show output here -->
