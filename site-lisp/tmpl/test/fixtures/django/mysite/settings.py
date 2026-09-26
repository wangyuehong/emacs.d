from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent.parent
TEMPLATES = [
    {"BACKEND": "django.template.backends.django.DjangoTemplates",
     "DIRS": [BASE_DIR / "templates"], "APP_DIRS": True},
    {"BACKEND": "django.template.backends.jinja2.Jinja2",
     "DIRS": [], "APP_DIRS": True},
]
