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
package cz.cvut.kbss.jopa.owl2java.prefix;

import cz.cvut.kbss.jopa.owl2java.Constants;
import org.semanticweb.owlapi.model.IRI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Optional;

/**
 * Resolves ontology prefixes using the <a href="https://prefix.cc">prefix.cc</a> service reverse lookup.
 */
public class PrefixCcRemotePrefixResolver implements RemotePrefixResolver {

    private static final Logger LOG = LoggerFactory.getLogger(PrefixCcRemotePrefixResolver.class);

    private static final String URL = "https://prefix.cc/reverse?format=ini&uri=";

    private final HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build();

    @Override
    public Optional<String> resolvePrefix(IRI ontologyIri) {
        LOG.trace("Attempting to resolve prefix for IRI <{}> via prefix.cc", ontologyIri);
        try {
            final HttpRequest req = HttpRequest.newBuilder(URI.create(URL + ontologyIri.getIRIString())).GET()
                                               .timeout(Constants.PREFIX_RESOLVE_TIMEOUT).build();
            final HttpResponse<String> resp = client.send(req, HttpResponse.BodyHandlers.ofString());
            if (resp.statusCode() != 200) {
                LOG.debug("Prefix for ontology IRI <{}> not found.", ontologyIri);
                return Optional.empty();
            }
            final String[] parts = resp.body().split("=");
            assert parts.length == 2;
            return Optional.of(parts[0]);
        } catch (IOException | InterruptedException e) {
            LOG.error("Unable to resolve prefix for ontology IRI <{}>.", ontologyIri, e);
            return Optional.empty();
        }
    }
}
