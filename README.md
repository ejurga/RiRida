# An R-interface for the IRIDA API 

This is an R package that wraps the IRIDA API. 

Currently, it supports querying an IRIDA project for its samples, and 
can retrieve sequence information for a given sample. 

## Configuration 

To access the IRIDA api, you will need an API client ID and API secret, as 
well as your username and password. Ask your IRIDA system administrator for 
these. In addition, you will need to supply the irida API link. These read into 
the package via a configuration file, by default `config.yml` in the root 
directory: 

```{yaml}
default:
  irida-api-url:
  irida-user: 
  irida-api-client: 
  irida-api-secret: 
```

You can supply the path to your own configuration file from R: 

```{r}
options(RiRida.config_path = "/path/to/config.yml")
```

## Usage 

To retrieve all samples and basic metadata from a project that you have 
permission to access, supply the IRIDA project ID (the _numerical_ ID, e.g., 123)


```{r}
project_samples(project_id = 123)
```

To retrieve the sequences associated with a sample, supply a sample ID (or 
vector of sample IDs). Note that this function queries an IRIDA API in parallel 
to speed up retrieval if you have a lot of samples to investigate. To limit (or 
speed up) retrieval, set the `n_con` parameter to an appropriate number of 
simultaneous connections. 

```{r}
get_all_sequence_info(samples = c(1,2,3,4,5), n_con = 10)
```

This function formats the response from the API into a dataframe. In case you 
want more fine control over e.g., the formatting of the response, you can 
use some helper functions along with `httr2` functions. For example, for 
sequence retrieval of a single IRIDA sample: 

```{r}
req_irida_sequences(sample_id = 12, type = "all") |> # Helper function
    httr2::req_perform() |> 
    httr2::resp_body_json() # httr2 functions for API interaction
```

Here, the details of accessing the IRIDA api are wrapped into
`req_irida_sequences()`. Using `type = "all"` requests a list of all sequences
associated with the sample, while `type = "pairs` requests only paired end
sequences. The rest of the API call and retrieval can be handled by the user
using `httr2` functions. 








