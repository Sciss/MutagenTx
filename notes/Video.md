- DVD SD PAL 16:9 is 720 x 576, but pixel's are not square (1024 x 576 is 16:9 for square pixels)
- test is players can just eat mp4 files? Then we could use Full HD, e.g.:
- http://owenmundy.com/blog/2013/01/use-processing-and-ffmpeg-to-export-hd-video/

    avconv -y -start_number 01500 -i 'movie%05d.png' -vcodec libx264 -r 25 -q 100 -pass 1 -s 1920x1080 -vb 6M -threads 0 -f mp4 out.mp4

    avconv -y -i 'movie%05d.png' -vcodec libx264 -r 25 -q 100 -pass 1 -s 1920x1080 -vb 6M -threads 0 -f mp4 -vf "transpose=2" out.mp4

- cf. http://askubuntu.com/questions/83711/how-can-i-rotate-a-video

- http://askubuntu.com/questions/262631/how-can-i-convert-video-to-divx4-to-play-on-my-kogan-tv

