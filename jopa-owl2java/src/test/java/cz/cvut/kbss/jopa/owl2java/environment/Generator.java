/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owl2java.environment;

import java.net.URI;
import java.util.Random;

public class Generator {


    private static final Random RANDOM = new Random();

    public static final String IRI_BASE = "https://onto.fel.cvut.cz/ontologies/jopa/owl2java/";

    public static URI generateUri() {
        return URI.create(IRI_BASE + RANDOM.nextInt());
    }
}
