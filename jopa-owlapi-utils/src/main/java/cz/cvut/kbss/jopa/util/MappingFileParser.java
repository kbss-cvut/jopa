/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class MappingFileParser {

	public static Map<URI, URI> getMappings(final File mf) {
		final Map<URI, URI> map = new HashMap<URI, URI>();
		String line = null;
		final File defaultDir = mf.getParentFile();
		BufferedReader r;
		try {
			r = new BufferedReader(new InputStreamReader(
					new FileInputStream(mf)));
			while ((line = r.readLine()) != null) {
				final StringTokenizer t = new StringTokenizer(line, ">");
				if (t.countTokens() != 2) {
					System.out
							.println("Ignoring line '" + line
									+ "' - invalid number of tokens="
									+ t.countTokens());
					continue;
				}

				final String uriName = t.nextToken().trim();
				final String fileName = t.nextToken().trim();
				final File actualFile = (new File(fileName).isAbsolute()) ? new File(
						fileName)
						: new File(defaultDir, fileName);

				map.put(URI.create(uriName), actualFile.toURI());
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return map;
	}
}
