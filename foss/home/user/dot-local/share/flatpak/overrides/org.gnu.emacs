# -*- coding: utf-8 -*-
# SPDX-License-Identifier: AGPL-3.0-only
# SPDX-FileCopyrightText: Copyright (C) 2024 Rifa Ilyasa Achrinza
# SPDX-FileNotice: <text>
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, version 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# </text>

[Context]
filesystems=!host;/home/user/Android/sdk:ro;/home/user/Documents/git-repos/achrinza/dotfiles/foss/home/user/dot-var/app/org.gnu.emacs/config/emacs:ro;/home/user/Documents/git-repos/achrinza/notes;
persistent=.gradle;.android;

[Environment]
ANDROID_HOME=/home/user/.android/sdk/
FLATPAK_ENABLE_SDK_EXT=node20,openjdk21,clojure,clojure-lsp,leiningen,android-cli-tools,babashka
