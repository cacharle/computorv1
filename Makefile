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

SRC_DIR = src
BUILD_DIR = build
NAME = computor

SRC = $(shell find $(SRC_DIR) -type f -name "*.hs")

all: $(NAME)

$(NAME): $(SRC)
	$(CC) --make -outputdir $(BUILD_DIR) -o $(NAME) $(SRC)

clean:
	$(RM) $(BUILD_DIR)/*.o $(BUILD_DIR)/*.hi

fclean: clean
	$(RM) $(NAME)

re: fclean all
