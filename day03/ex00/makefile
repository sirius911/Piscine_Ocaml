# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: clorin <clorin@student.42.fr>              +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2023/07/25 11:25:31 by clorin            #+#    #+#              #
#    Updated: 2023/07/25 15:09:34 by clorin           ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

RESULT = testsdl
SOURCES = ft_graphics.ml
LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lGraphics
OCAMLMAKEFILE = ../../OCamlMakefile
THREADS = true
include $(OCAMLMAKEFILE)