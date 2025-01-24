#!/usr/bin/env python
import argparse
import contextlib
import json
import os
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


TrackId = str
Url = str


@dataclass
class PlaylistInfo:
    title: str
    thumbnail: Url | None
    tracks: list[TrackId]


def fetch_playlist_info(url: str) -> PlaylistInfo:
    """Fetch the playlist title using yt-dlp."""
    result = subprocess.run(
        ["yt-dlp", "--flat-playlist", "--dump-single-json", url],
        capture_output=True,
        text=True,
    )
    playlist = json.loads(result.stdout)

    title = playlist["title"].replace("/", "\\").replace(" ", "_").lower()
    thumbnail_info = playlist["thumbnails"]
    if thumbnail_info:
        thumbnail = max(playlist["thumbnails"], key=lambda item: item["height"])["url"]
    else:
        thumbnail = None
    tracks = [entry["id"] for entry in playlist["entries"]]
    
    return PlaylistInfo(
        title=title,
        thumbnail=thumbnail,
        tracks=tracks
    )



def download_track(track_id: str, output_path: str):
    """Download a single track from YouTube."""
    if os.path.exists(output_path):
        print(f"{track_id} already downloaded")
        return f"file {output_path}"

    subprocess.run(
        [
            "yt-dlp",
            "--embed-metadata",
            "-x",
            "--audio-format",
            "mp3",
            track_id,
            "-o",
            output_path,
        ]
    )


def main(url: str, part_size: int | None):
    """Main function to download and process a YouTube playlist."""
    playlist = fetch_playlist_info(url)
    print(f"Detected playlist {playlist.title} with {len(playlist.tracks)} tracks")

    thumbnail_path = None
    if playlist.thumbnail:
        print("Downloading thumbnail")
        thumbnail_path = "thumbnail.png"
        subprocess.run(["wget", playlist.thumbnail, "-O", thumbnail_path], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    os.makedirs(playlist.title, exist_ok=True)
    base_path = Path(playlist.title)
    os.makedirs(base_path / "source", exist_ok=True)

    playlists_parts = [[]]
    current_part_size = 0
    current_playlist_part_idx = 0
    for idx, track_id in enumerate(playlist.tracks):
        status = f"[{idx+1}/{len(playlist.tracks)}]"
        output_path = base_path / f"source/{track_id}.mp3"
        if not output_path.exists():
            download_track(track_id, output_path)
            print(f"{status} Downloaded")
        else:
            print(f"{status} Cached")
        size = os.path.getsize(output_path) >> 20

        if part_size is not None and current_part_size + size > part_size:
            current_playlist_part_idx += 1
            current_part_size = size
        else:
            current_part_size += size

        if current_playlist_part_idx >= len(playlists_parts):
            playlists_parts.append([])

        playlists_parts[current_playlist_part_idx].append(output_path)

    for idx, playlist_part_files in enumerate(playlists_parts):
        print(f"Merging part {idx+1}/{len(playlists_parts)}")
        with open("playlist.txt", "w") as f:
            f.write("\n".join((f"file {path}" for path in playlist_part_files)))

        # Concatenate tracks
        subprocess.run(
            [
                "ffmpeg",
                "-f",
                "concat",
                "-safe",
                "0",
                "-i",
                "playlist.txt",
                "-c",
                "copy",
                "output_buf.mp3",
            ],
            stderr=subprocess.DEVNULL,
        )

        # Add thumbnail and finalize
        additional_inputs = []
        if thumbnail_path is not None:
            additional_inputs = ["-i", thumbnail_path]
            
        subprocess.run(
            [
                "ffmpeg",
                "-i",
                "output_buf.mp3",
                *additional_inputs,
                "-map",
                "0",
                "-map",
                "1",
                "-c",
                "copy",
                "-id3v2_version",
                "3",
                "-metadata:s:v",
                "title=Album cover",
                "-metadata:s:v",
                "comment=Cover (Front)",
                base_path
                / (
                    f"{playlist.title}.mp3"
                    if part_size is None
                    else f"{playlist.title}_part{idx+1}.mp3"
                ),
            ],
            stderr=subprocess.DEVNULL,
        )

        with contextlib.suppress(FileNotFoundError):
            os.remove("output_buf.mp3")

    print("Done")


def cli() -> None:
    """Command-line interface for the script."""
    parser = argparse.ArgumentParser(
        description="Download and process YouTube playlist"
    )
    parser.add_argument("url", help="YouTube playlist URL")
    parser.add_argument(
        "-s",
        "--part-size",
        help="Max size of each part of the playlist",
        default=None,
    )

    args = parser.parse_args()

    print(f"Downloading playlist from {args.url=} with part size {args.part_size}MB")
    main(args.url, int(args.part_size))


if __name__ == "__main__":
    cli()
