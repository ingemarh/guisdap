from pathlib import Path

import healpy as hp
import matplotlib.pyplot as plt
import numpy as np
from astropy.visualization import astropy_mpl_style, quantity_support

plt.style.use(astropy_mpl_style)
quantity_support()


class HaslamSkyMap:
    def __init__(self, data_path):
        """
        Initialize the HaslamSkyMap class.

        Parameters:
        - data_path: Path to the HAS data file (string or Path object)
        """
        self.data_path = Path(data_path).expanduser()
        self.has_data, self.header = hp.read_map(self.data_path, h=True)
        self.nside = hp.get_nside(self.has_data)
        self.tunit = "K"
        self.version = "HAS"
        self._parse_header()

    def _parse_header(self):
        """
        Parse the header information to extract metadata such as TUNIT and version.
        """
        for i, (k, v) in enumerate(self.header):
            if k == "TUNIT1":
                self.tunit = v
            if k == "DATE":
                self.version += v[2:4]

    def print_header_info(self):
        """
        Print the header information of the HAS data file.
        """
        for i, (k, v) in enumerate(self.header):
            print(f"{k:8} {v}")

    def print_basic_info(self):
        """
        Print basic information about the HAS data.
        """
        print("Nside:", self.nside)  # HEALPix grid resolution
        print("Min:", self.has_data.min(), "Max:", self.has_data.max())

    def visualize(self):
        """
        Visualize the HAS data using a Mollweide projection.
        """
        hp.mollview(
            np.log10(self.has_data),
            coord="G",
            title=f"{self.version} Haslam 408 MHz All-sky Map",
            unit=f"log10({self.tunit})",
            cmap="inferno",
        )
        plt.show()
