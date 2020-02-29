# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: cacharle <marvin@42.fr>                    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2020/02/29 11:54:31 by cacharle          #+#    #+#              #
#    Updated: 2020/02/29 12:00:11 by cacharle         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

CC = ghc

NAME = computor

SRC = $(shell find . -name "*.hs")

all: $(NAME)

$(NAME):
	$(CC) -o $(NAME) $(SRC)

clean:
	rm -f *.o *.hi

fclean: clean
	rm -f $(NAME)

re: fclean all
