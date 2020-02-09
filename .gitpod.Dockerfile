
FROM gitpod/workspace-full

USER gitpod
# Installing Haskell
RUN sudo bash -c "curl -sSL https://get.haskellstack.org/ | sh"