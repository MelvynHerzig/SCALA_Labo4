document.addEventListener('DOMContentLoaded', function () {
    const socket = new WebSocket("ws://" + location.host + "/subscribe");
    socket.onmessage = function (ev) {
        boardMessage.innerHTML = ev.data
    }
}, false);

/**
 * JS to handle message form submit
 */
function submitMessageForm() {
    fetch(
        "/send",
        {
            method: "POST",
            body: JSON.stringify({msg: messageInput.value})
        }
    ).then(response => response.json())
        .then(json => {
            if (!json.err) {
                messageInput.value = ""
            }
            errorDiv.innerText = json.err
            errorDiv.style.display = "block";;
        })
}

/**
 * JS to handle login form submit
 */
function submitLoginForm() {
    fetch(
        "/login",
        {
            method: 'POST',
            redirect: 'follow',
            body: JSON.stringify({username: loginInput.value})
        }
    )
        .then(response => {
            if (response.redirected) {
                window.location.href = response.url;
            } else if (response.status === 400 || response.status === 404) {
                errorLogin.style.display = "block";
                response.json().then(msg => errorLogin.innerText = msg);
            }
        })
}

/**
 * JS to handle registration form submit
 */
function submitRegistrationForm() {
    fetch(
        "/register",
        {
            method: 'POST',
            redirect: 'follow',
            body: JSON.stringify({username: registerInput.value})
        }
    )
        .then(response => {
            if (response.redirected) {
                window.location.href = response.url;
            } else if (response.status === 400) {
                errorRegister.style.display = "block";
                response.json().then(msg => errorRegister.innerText = msg);
            }
        })
}