# Main author Wang Yonghui, IGGCAS, China
from datetime import datetime, timezone

import astropy.units as u
import h5py
import numpy as np
from astropy.coordinates import EarthLocation
from numpy.lib import recfunctions as rfn

from haslamskymap import HaslamSkyMap
from skymapcal import TskyCalculator


class AddTskyToH5:
    def __init__(self, has_data_path):
        """
        Initialize the AddTskyToH5 class.

        Parameters:
        - h5_file_path: Path to the HDF5 file
        - has_data_path: Path to the HAS data file
        - location: Observation location (EarthLocation object)
        """
        self.haslam_map = HaslamSkyMap(has_data_path)
        self.sky_calculator = TskyCalculator(self.haslam_map.has_data)
        self.loc = {
            "S": EarthLocation(lat=18.3492 * u.deg, lon=109.6222 * u.deg, height=50 * u.m),
            "W": EarthLocation(lat=19.5982 * u.deg, lon=110.7908 * u.deg, height=25 * u.m),
            "D": EarthLocation(lat=19.5281 * u.deg, lon=109.1322 * u.deg, height=100 * u.m),
            "V": EarthLocation(lat=69.863 * u.deg, lon=19.21 * u.deg, height=30 * u.m),
            "T": EarthLocation(lat=69.863 * u.deg, lon=19.21 * u.deg, height=30 * u.m),
            "L": EarthLocation(lat=78.153 * u.deg, lon=16.029 * u.deg, height=438 * u.m),
        }

    def calculate_tsky(self, az, el, obs_time, obs_loc):
        """
        Calculate the Tsky value for given azimuth, elevation, and observation time.

        Parameters:
        - az: Azimuth angle (in degrees)
        - el: Elevation angle (in degrees)
        - obs_time: Observation time (ISO format string)

        Returns:
        - tsky: Sky brightness value
        """
        return self.sky_calculator.Tsky(az, el, obs_time, obs_loc)

    def get_tsky(self, site, unixt, az, el):
        """
        Add the Tsky field to the structured array in the HDF5 file.
        """

        # Convert observation time to ISO format (example implementation)
        obs_dt = [datetime.fromtimestamp(ut, tz=timezone.utc) for ut in unixt]

        obs_loc = self.loc[site]

        # Calculate Tsky for each entry
        tsky_values = np.array(
            [
                self.calculate_tsky(a, e, t, obs_loc)
                for a, e, t in zip(az,el, obs_dt)
            ],
            dtype=np.float32,
        )

        if self.haslam_map.tunit == "mK":
            tsky_values /= 1000
        return tsky_values

    def add_tsky_field(self, h5_file_path, after_field="fs"):
        """
        Add the Tsky field to the structured array in the HDF5 file.
        """
        with h5py.File(h5_file_path, "r+") as h5_file:
            # Read the dataset
            head = h5_file["/head"][...]

            # Extract azimuth, elevation, and observation time
            az = head["az"]
            el = head["el"]
            unixt = head["dt"] / 1_000_000

            # Convert observation time to ISO format (example implementation)
            obs_dt = [datetime.fromtimestamp(ut, tz=timezone.utc) for ut in unixt]

            loc_ind = h5_file_path.stem[-2]
            print(h5_file_path.name, loc_ind)
            obs_loc = self.loc[loc_ind]

            # Calculate Tsky for each entry
            tsky_values = np.array(
                [
                    self.calculate_tsky(a, e, t, obs_loc)
                    for a, e, t in zip(az, el, obs_dt)
                ],
                dtype=np.float32,
            )

            if self.haslam_map.tunit == "mK":
                tsky_values /= 1000

            # Append the Tsky field
            data_with_tsky = rfn.append_fields(head, "tsky", tsky_values, usemask=False)

            # Reorder fields to place Tsky after the specified field
            field_names = list(data_with_tsky.dtype.names)
            tsky_index = field_names.index("tsky")
            after_index = field_names.index(after_field)
            reordered_fields = (
                field_names[: after_index + 1]
                + ["tsky"]
                + field_names[after_index + 1 : tsky_index]
                + field_names[tsky_index + 1 :]
            )
            head_reordered = data_with_tsky[reordered_fields]

            # Overwrite the dataset with the updated array
            del h5_file["/head"]
            h5_file.create_dataset("/head", data=head_reordered)
