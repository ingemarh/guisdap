import setuptools

with open("README.txt", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setuptools.setup(
    name="lpgen",
    version="1.0.0",
    author="Ingemar Häggström",
    author_email="ingemar@eiscat.se",
    description="EISCAT Lag Profiling",
    url="https://github.com/ingemarh/lpgen",
    #packages=setuptools.find_packages(where="."),
    #packages=setuptools.find_packages(include="par_gen"),
    python_requires=">=3.6",
)
