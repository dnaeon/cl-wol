# wol

The WoL application wakes up remote systems identified by their MAC addresses by
broadcasting a magic packet

## Usage

``` shell
wol [global-options] [<command>] [command-options] [arguments ...]
```

## Options

`wol` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

## Sub Commands

`wol` provides the following sub commands:

``` shell
  add-host         add host to the database
  delete-host      delete host(s) from the database
  init-db          init local database file
  list-hosts       displays the hosts from the database
  print-doc        print the documentation
  wake             wakes up remote systems
  zsh-completions  generate the Zsh completions script

```

## Authors

* Marin Atanasov Nikolov <dnaeon@gmail.com>

## License

BSD 2-Clause

# wol add-host

`wol add-host` -- add host to the database

## Usage

``` shell
wol [global-options] add-host [options] [arguments ...]
```

## Options

`wol add-host` accepts the following options:

``` shell
      --help             display usage information and exit
      --version          display version and exit
  -a, --address <VALUE>  MAC address of the host
  -d, --database <PATH>  path to the database file [env: $DATABASE]
  -n, --name <VALUE>     name of the host to add

```

## Examples

Add a new host:

``` shell
wol add-host --database wol.db --name box-01 --address 00:01:02:03:04:05
```

# wol delete-host

`wol delete-host` -- delete host(s) from the database

## Usage

``` shell
wol delete-host NAME ...
```

## Options

`wol delete-host` accepts the following options:

``` shell
      --help             display usage information and exit
      --version          display version and exit
  -d, --database <PATH>  path to the database file [env: $DATABASE]

```

## Examples

Delete hosts from the db:

``` shell
wol delete-host --database wol.db box-01 box-02
```

# wol init-db

`wol init-db` -- init local database file

## Usage

``` shell
wol [global-options] init-db [options] [arguments ...]
```

## Options

`wol init-db` accepts the following options:

``` shell
      --help             display usage information and exit
      --version          display version and exit
  -d, --database <PATH>  path to the database file [env: $DATABASE]

```

## Examples

Initialize a new database:

``` shell
wol init-db --database wol.db
```

# wol list-hosts

`wol list-hosts` -- displays the hosts from the database

## Usage

``` shell
wol [global-options] list-hosts [options] [arguments ...]
```

## Options

`wol list-hosts` accepts the following options:

``` shell
      --help             display usage information and exit
      --version          display version and exit
  -d, --database <PATH>  path to the database file [env: $DATABASE]
  -l, --limit <INT>      max number of rows to fetch [default: 20]
  -o, --offset <INT>     offset to use when fetching rows [default: 0]

```

## Examples

List hosts from database:

``` shell
wol list-hosts --database wol.db
```

List 50 hosts at max:

``` shell
wol list-hosts --database wol.db --limit 50
```

# wol print-doc

`wol print-doc` -- print the documentation

## Usage

``` shell
wol print-doc 
```

## Options

`wol print-doc` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# wol wake

`wol wake` -- wakes up remote systems

## Usage

``` shell
wol wake [options] MAC-ADDRESS ...
```

## Options

`wol wake` accepts the following options:

``` shell
      --help              display usage information and exit
      --password <VALUE>  optional SecureOn password to send
      --version           display version and exit
  -a, --address <VALUE>   broadcast address to send the magic packet to [default:
                          255.255.255.255]
  -d, --database <PATH>   path to the database file [env: $DATABASE]
  -n, --name <ITEM>       host to lookup from the database and wake
  -p, --port <INT>        UDP port to send broadcast packet to [default: 7]

```

## Examples

Wake up a single host:

``` shell
wol wake 00:01:02:03:05
```

Wake up multiple hosts:

``` shell
wol wake 00:01:02:03:04:05 aa:bb:cc:dd:ee:ff
```

Wake up hosts by name from db:

``` shell
wol wake --database wol.db --name box-01 --name box-02
```

# wol zsh-completions

`wol zsh-completions` -- generate the Zsh completions script

## Usage

``` shell
wol zsh-completions 
```

## Options

`wol zsh-completions` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

