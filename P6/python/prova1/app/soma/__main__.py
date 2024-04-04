import sys
from . import soma

def main():
    if len(sys.argv) != 3:
        print("Uso: python -m app.soma <arg1> <arg2>")
        sys.exit(1)

    a = float(sys.argv[1])
    b = float(sys.argv[2])

    print(f"{a} + {b} = {soma(a, b)}\n")

if __name__ == "__main__":
    main()
