# iwr -useb https://raw.githubusercontent.com/ajgrf/dotfiles/windows/bin/setup-windows.ps1 | iex

function Setup-Scoop {
    $packages = (
        "alacritty",
        "aria2c",
        "auto-dark-mode-np",
        "bugn",
        "git",
        "hwinfo",
        "nextcloud",
        "sudo",
        "topgrade",
        "ueli",
        "windowsspyblocker"
    )

    if (! (Test-Path -Path "$env:USERPROFILE\scoop")) {
        Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')

        scoop alias add upgrade 'scoop update *'
        scoop install git
        scoop bucket add extras
        scoop bucket add nonportable
        scoop bucket add ajgrf https://github.com/ajgrf/scoop-bucket.git

        scoop install @packages
    }
}

function Setup-Chocolatey {
    $True
}

function Setup-Dotfiles {
    $True
}

Setup-Scoop
Setup-Chocolatey
Setup-Dotfiles
