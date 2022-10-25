FROM testcab/yay

RUN sudo pacman-key --init; sudo pacman --noconfirm -Syyu; sudo pacman --noconfirm --needed -S base-devel git vim cmake glibc

RUN yay --noconfirm -S ydotool

RUN mkdir tcm-test; cd tcm-test

RUN git clone https://github.com/sstephenson/bats lib/bats
RUN git clone https://github.com/ztombol/bats-support lib/bats-support
RUN git clone https://github.com/ztombol/bats-file lib/bats-file
RUN git clone https://github.com/ztombol/bats-assert lib/bats-assert

RUN ls /home/makepkg
RUN ls /home/makepkg/tcm-test

CMD ./lib/bats/bin/bats "test_terminal_config_manager.sh"