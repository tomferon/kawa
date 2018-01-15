Kawa is a small command-line tool to manage some key-value store in a single
file. It manages files containing key-value pairs like the following. These
files are meant to be human-readable.

```
hostname = localhost
port = 5432
username = myuser
password = secret
database = mydb
```

There are three commands supported by the executable: `get`, `set` and `replace`
which, respectively, gets the value assigned to a key from a store, sets a new
value to a key in a store and replaces all occurences of keys by their values in
a file.

This can be used to generate a file from a template and some secrets contained
in such a store. For instance, a provisioning tool could create a file
`config.template.yml` for some application containing its configuration with
lines like `username: USERNAME` and the variables would be stored in
`secrets.kawa`. Then, before launching the application, a systemd service could
run `kawa replace secrets.kawa -i config.template.yml -o config.yml`.

For more information, run `kawa --help`.