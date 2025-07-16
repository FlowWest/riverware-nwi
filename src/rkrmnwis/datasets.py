from importlib import resources
import pandas as pd
from functools import lru_cache


class DataLoader:
    @staticmethod
    @lru_cache(maxsize=None)
    def _load_csv(filename):
        """Loads a single csv from the data path in this package"""
        try:
            files = resources.files("rkrmnwis.data")
            with files.joinpath(filename).open("r", encoding="UTF-8") as f:
                return pd.read_csv(f)
        except Exception as e:
            raise (e)

    @staticmethod
    def swe_station():
        return DataLoader._load_csv("station_triplets.csv")


def swe_stations():
    return DataLoader.swe_station()
