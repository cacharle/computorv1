# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: cacharle <marvin@42.fr>                    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2020/02/29 11:54:31 by cacharle          #+#    #+#              #
#    Updated: 2020/02/29 12:01:42 by cacharle         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

CC = ghc

NAME = computor

SRC = $(shell find . -name "*.hs")

all: $(NAME)

$(NAME): $(SRC)
	$(CC) -o $(NAME) $(SRC)

clean:
	rm -f *.o *.hi

fclean: clean
	rm -f $(NAME)

re: fclean all
