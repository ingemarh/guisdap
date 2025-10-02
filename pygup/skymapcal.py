from astropy.coordinates import AltAz, SkyCoord
from astropy.time import Time
import numpy as np
import healpy as hp
import astropy.units as u

class TskyCalculator:
    def __init__(self, has_data):
        """
        Initialize the TskyCalculator class.

        Parameters:
        - location: Geographic location (an instance of astropy.coordinates.EarthLocation)
        - has_data: HEALPix data array
        """
        self.nside = hp.get_nside(has_data)
        self.has_data = has_data

    def Tsky(self, az, el, obs_dt, obs_loc):
        """
        Calculate the sky noise at radar beam (az, el).

        Parameters:
        - az: Azimuth angle (in degrees)
        - el: Elevation angle (in degrees)
        - obs_time: Observation time (datetime)

        Returns:
        - Tsky: Sky noise value
        """
        # Convert to AltAz coordinates
        az *= u.deg
        el *= u.deg

        obsdt_ios = obs_dt.strftime("%Y-%m-%dT%H:%M:%S.%f")

        obs_time = Time(obsdt_ios, format="isot", scale="utc")
        altaz = AltAz(az=az, alt=el, obstime=obs_time, location=obs_loc)
        sky_coord = SkyCoord(altaz)

        # Transform from ICRS to Galactic coordinates
        sky_coord_gal = sky_coord.galactic

        # Lookup value from HEALPix data
        theta = 0.5 * np.pi - sky_coord_gal.b.radian  # θ = π/2 - latitude
        phi = sky_coord_gal.l.radian  # longitude

        pix = hp.ang2pix(self.nside, theta, phi)
        tsky = self.has_data[pix]
        return tsky
