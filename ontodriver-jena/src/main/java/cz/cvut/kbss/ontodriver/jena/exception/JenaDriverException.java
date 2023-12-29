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
package cz.cvut.kbss.ontodriver.jena.exception;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Generic exception for the Jena driver.
 */
public class JenaDriverException extends OntoDriverException {

    public JenaDriverException(String message) {
        super(message);
    }

    public JenaDriverException(Throwable cause) {
        super(cause);
    }

    public JenaDriverException(String message, Throwable cause) {
        super(message, cause);
    }
}
