# Main author Wang Yonghui, IGGCAS, China
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path


from addtsky import AddTskyToH5


class ParallelAddTsky:
    def __init__(self, has_fpath):
        """
        Initialize the ParallelAddTsky class.

        Parameters:
        - has_fpath: Path to the HAS data file
        - location: Observation location (EarthLocation object)
        """
        self.add_tsky = AddTskyToH5(has_fpath)

    def process_h5_file(self, h5_fp):
        """
        Process a single HDF5 file to add the Tsky field.

        Parameters:
        - h5_fp: Path to the HDF5 file
        """
        print(f"Processing: {h5_fp.name}")
        self.add_tsky.add_tsky_field(h5_fp)

    def process_input(self, site, ut, az, el):
        """
        Process vectors of ut,az,el and get Tsky.

        Parameters:
        - site code and vectors of ut,az,el
        """
        return self.add_tsky.get_tsky(site,ut,az,el)

    def process_h5_files_in_parallel(self, h5_dir, num_workers=4):
        """
        Process all HDF5 files in a directory in parallel.

        Parameters:
        - h5_dir: Directory containing HDF5 files
        - num_workers: Number of parallel workers (default: 4)
        """
        h5_files = sorted(Path(h5_dir).glob("*.h5"))
        print(f"Found {len(h5_files)} HDF5 files to process.")

        with ProcessPoolExecutor(max_workers=num_workers) as executor:
            executor.map(self.process_h5_file, h5_files)

def matface(datapath,site,ut,az,el):
    has_fname = "lambda_haslam408_nofilt.fits"
    has_fpath = Path(datapath) / has_fname
    parallel_add_tsky = ParallelAddTsky(has_fpath)
    if site=='3': site='S'
    return parallel_add_tsky.process_input(site,ut,az,el)

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <h5_dir_suffix>")
        sys.exit(1)

    h5_dir_suffix = sys.argv[1]

    data_dir = Path("~/data/HAS.data").expanduser()
    has_fname = "lambda_haslam408_nofilt.fits"
    has_fpath = data_dir / has_fname

    parallel_add_tsky = ParallelAddTsky(has_fpath)

    h5_dir = Path(f"/Volumes/SN580/ISRData/SYISR/L1/{h5_dir_suffix}")

    parallel_add_tsky.process_h5_files_in_parallel(h5_dir, num_workers=8)

    # add_tsky = AddTskyToH5(has_fpath, syisr)

    # h5_fps = h5_fp.glob("*.h5")

    # for i, h5_fp in enumerate(sorted(h5_fps)):
    #     print(i, h5_fp.name)
    #     add_tsky.add_tsky_field(h5_fp)


if __name__ == "__main__":
    main()
