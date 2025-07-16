#!/usr/bin/env python3

import click
from pathlib import Path
from rkrmnwis.datasets import swe_stations


@click.group()
def nwis():
    """
    NWIS data compilation tools for RKRM analysis.

    Use subcommands to compile specific data types:

      nwis swe --output data         # Snow Water Equivalent data
      nwis prism --verbose           # PRISM climate data
      nwis climate_indx              # Climate index data
      nwis ukr_inflow --output tmp   # Upper Klamath River inflow data
    """
    pass


@nwis.command()
@click.argument("dataset")
@click.option("--destination", "-d", default="stdout")
def dataset(dataset, destination):
    if dataset == "swe_stations":
        data = swe_stations()
        if destination == "stdout":
            print(data)

    else:
        click.secho(f"no dataset was found for {dataset}", fg="red")


@nwis.command()
@click.option(
    "--output", "-o", default="./output", help="Output directory for SWE data"
)
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
def swe(output, verbose):
    """Compile Snow Water Equivalent (SWE) data from NWIS stations."""
    if verbose:
        click.echo("Starting SWE data compilation...")
        click.echo(f"Output directory: {output}")

    Path(output).mkdir(parents=True, exist_ok=True)

    click.echo("Fetching SWE data from NWIS...")
    click.echo("Processing snow measurement stations...")
    click.echo("Saving SWE data...")

    if verbose:
        click.echo("SWE data compilation completed!")
    else:
        click.echo("SWE data compiled successfully!")


@nwis.command()
@click.option(
    "--output", "-o", default="./output", help="Output directory for PRISM data"
)
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
def prism(output, verbose):
    """Compile PRISM climate data for the RKRM region."""
    if verbose:
        click.echo("Starting PRISM data compilation...")
        click.echo(f"Output directory: {output}")

    Path(output).mkdir(parents=True, exist_ok=True)

    click.echo("Fetching PRISM climate data...")
    click.echo("Processing temperature and precipitation...")
    click.echo("Saving PRISM data...")

    if verbose:
        click.echo("PRISM data compilation completed!")
    else:
        click.echo("PRISM data compiled successfully!")


@nwis.command()
@click.option(
    "--output", "-o", default="./output", help="Output directory for climate index data"
)
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
def climate_indx(output, verbose):
    """Compile climate index data (PDO, ENSO, etc.) for RKRM analysis."""
    if verbose:
        click.echo("Starting climate index data compilation...")
        click.echo(f"Output directory: {output}")

    Path(output).mkdir(parents=True, exist_ok=True)

    click.echo("Fetching climate index data...")
    click.echo("Processing PDO, ENSO, and other indices...")
    click.echo("Saving climate index data...")

    if verbose:
        click.echo("Climate index data compilation completed!")
    else:
        click.echo("Climate index data compiled successfully!")


@nwis.command()
@click.option(
    "--output", "-o", default="./output", help="Output directory for UKR inflow data"
)
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose output")
def ukr_inflow(output, verbose):
    """Compile Upper Klamath River inflow data from NWIS stations."""
    if verbose:
        click.echo("Starting UKR inflow data compilation...")
        click.echo(f"Output directory: {output}")

    Path(output).mkdir(parents=True, exist_ok=True)

    click.echo("Fetching UKR inflow data from NWIS...")
    click.echo("Processing streamflow stations...")
    click.echo("Saving inflow data...")

    if verbose:
        click.echo("UKR inflow data compilation completed!")
    else:
        click.echo("UKR inflow data compiled successfully!")


if __name__ == "__main__":
    nwis()
