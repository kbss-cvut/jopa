/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.environment;

import java.net.URI;
import java.util.Random;

public class Generator {

    private static final String URI_BASE = "http://onto.fel.cvut.cz/ontologies/ontodriver/jena/";

    private static final Random RANDOM = new Random();

    public static URI generateUri() {
        return URI.create(URI_BASE + "instance" + RANDOM.nextInt());
    }

    public static int randomInt() {
        return RANDOM.nextInt();
    }

    public static int randomInt(int max) {
        return RANDOM.nextInt(max);
    }

    public static boolean randomBoolean() {
        return RANDOM.nextBoolean();
    }
}
